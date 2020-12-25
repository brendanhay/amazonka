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

import qualified Network.AWS.Config.Types.Name as Types
import qualified Network.AWS.Config.Types.RecordingGroup as Types
import qualified Network.AWS.Config.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the recording of configuration changes of an AWS resource.
--
-- /See:/ 'mkConfigurationRecorder' smart constructor.
data ConfigurationRecorder = ConfigurationRecorder'
  { -- | The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
    name :: Core.Maybe Types.Name,
    -- | Specifies the types of AWS resources for which AWS Config records configuration changes.
    recordingGroup :: Core.Maybe Types.RecordingGroup,
    -- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
    roleARN :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfigurationRecorder' value with any optional fields omitted.
mkConfigurationRecorder ::
  ConfigurationRecorder
mkConfigurationRecorder =
  ConfigurationRecorder'
    { name = Core.Nothing,
      recordingGroup = Core.Nothing,
      roleARN = Core.Nothing
    }

-- | The name of the recorder. By default, AWS Config automatically assigns the name "default" when creating the configuration recorder. You cannot change the assigned name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crName :: Lens.Lens' ConfigurationRecorder (Core.Maybe Types.Name)
crName = Lens.field @"name"
{-# DEPRECATED crName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the types of AWS resources for which AWS Config records configuration changes.
--
-- /Note:/ Consider using 'recordingGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRecordingGroup :: Lens.Lens' ConfigurationRecorder (Core.Maybe Types.RecordingGroup)
crRecordingGroup = Lens.field @"recordingGroup"
{-# DEPRECATED crRecordingGroup "Use generic-lens or generic-optics with 'recordingGroup' instead." #-}

-- | Amazon Resource Name (ARN) of the IAM role used to describe the AWS resources associated with the account.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRoleARN :: Lens.Lens' ConfigurationRecorder (Core.Maybe Types.String)
crRoleARN = Lens.field @"roleARN"
{-# DEPRECATED crRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON ConfigurationRecorder where
  toJSON ConfigurationRecorder {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("recordingGroup" Core..=) Core.<$> recordingGroup,
            ("roleARN" Core..=) Core.<$> roleARN
          ]
      )

instance Core.FromJSON ConfigurationRecorder where
  parseJSON =
    Core.withObject "ConfigurationRecorder" Core.$
      \x ->
        ConfigurationRecorder'
          Core.<$> (x Core..:? "name")
          Core.<*> (x Core..:? "recordingGroup")
          Core.<*> (x Core..:? "roleARN")
