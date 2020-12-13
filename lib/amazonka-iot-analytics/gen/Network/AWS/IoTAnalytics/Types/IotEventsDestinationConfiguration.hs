{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
  ( IotEventsDestinationConfiguration (..),

    -- * Smart constructor
    mkIotEventsDestinationConfiguration,

    -- * Lenses
    iedcInputName,
    iedcRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information for delivery of dataset contents to AWS IoT Events.
--
-- /See:/ 'mkIotEventsDestinationConfiguration' smart constructor.
data IotEventsDestinationConfiguration = IotEventsDestinationConfiguration'
  { -- | The name of the AWS IoT Events input to which dataset contents are delivered.
    inputName :: Lude.Text,
    -- | The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IotEventsDestinationConfiguration' with the minimum fields required to make a request.
--
-- * 'inputName' - The name of the AWS IoT Events input to which dataset contents are delivered.
-- * 'roleARN' - The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
mkIotEventsDestinationConfiguration ::
  -- | 'inputName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  IotEventsDestinationConfiguration
mkIotEventsDestinationConfiguration pInputName_ pRoleARN_ =
  IotEventsDestinationConfiguration'
    { inputName = pInputName_,
      roleARN = pRoleARN_
    }

-- | The name of the AWS IoT Events input to which dataset contents are delivered.
--
-- /Note:/ Consider using 'inputName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedcInputName :: Lens.Lens' IotEventsDestinationConfiguration Lude.Text
iedcInputName = Lens.lens (inputName :: IotEventsDestinationConfiguration -> Lude.Text) (\s a -> s {inputName = a} :: IotEventsDestinationConfiguration)
{-# DEPRECATED iedcInputName "Use generic-lens or generic-optics with 'inputName' instead." #-}

-- | The ARN of the role that grants AWS IoT Analytics permission to deliver dataset contents to an AWS IoT Events input.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iedcRoleARN :: Lens.Lens' IotEventsDestinationConfiguration Lude.Text
iedcRoleARN = Lens.lens (roleARN :: IotEventsDestinationConfiguration -> Lude.Text) (\s a -> s {roleARN = a} :: IotEventsDestinationConfiguration)
{-# DEPRECATED iedcRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON IotEventsDestinationConfiguration where
  parseJSON =
    Lude.withObject
      "IotEventsDestinationConfiguration"
      ( \x ->
          IotEventsDestinationConfiguration'
            Lude.<$> (x Lude..: "inputName") Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON IotEventsDestinationConfiguration where
  toJSON IotEventsDestinationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("inputName" Lude..= inputName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
