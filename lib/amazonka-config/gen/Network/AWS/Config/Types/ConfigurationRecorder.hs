{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConfigurationRecorder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationRecorder
  ( ConfigurationRecorder (..),

    -- * Smart constructor
    mkConfigurationRecorder,

    -- * Lenses
    crName,
    crRecordingGroup,
    crRoleARN,
  )
where

import Network.AWS.Config.Types.RecordingGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the recording of configuration changes of an AWS resource.
--
-- /See:/ 'mkConfigurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { name ::
      Lude.Maybe Lude.Text,
    recordingGroup :: Lude.Maybe RecordingGroup,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationRecorder' with the minimum fields required to make a request.
--
-- * 'name' - The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
-- * 'recordingGroup' - Specifies the types of AWS resources for which AWS Config records configuration changes.
-- * 'roleARN' - Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
mkConfigurationRecorder ::
  ConfigurationRecorder
mkConfigurationRecorder =
  ConfigurationRecorder'
    { name = Lude.Nothing,
      recordingGroup = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' ConfigurationRecorder (Lude.Maybe Lude.Text)
crName = Lens.lens (name :: ConfigurationRecorder -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ConfigurationRecorder)
{-# DEPRECATED crName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the types of AWS resources for which AWS Config records configuration changes.
--
-- /Note:/ Consider using 'recordingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRecordingGroup :: Lens.Lens' ConfigurationRecorder (Lude.Maybe RecordingGroup)
crRecordingGroup = Lens.lens (recordingGroup :: ConfigurationRecorder -> Lude.Maybe RecordingGroup) (\s a -> s {recordingGroup = a} :: ConfigurationRecorder)
{-# DEPRECATED crRecordingGroup "Use generic-lens or generic-optics with 'recordingGroup' instead." #-}

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoleARN :: Lens.Lens' ConfigurationRecorder (Lude.Maybe Lude.Text)
crRoleARN = Lens.lens (roleARN :: ConfigurationRecorder -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ConfigurationRecorder)
{-# DEPRECATED crRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ConfigurationRecorder where
  parseJSON =
    Lude.withObject
      "ConfigurationRecorder"
      ( \x ->
          ConfigurationRecorder'
            Lude.<$> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "recordingGroup")
            Lude.<*> (x Lude..:? "roleARN")
      )

instance Lude.ToJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("name" Lude..=) Lude.<$> name,
            ("recordingGroup" Lude..=) Lude.<$> recordingGroup,
            ("roleARN" Lude..=) Lude.<$> roleARN
          ]
      )
