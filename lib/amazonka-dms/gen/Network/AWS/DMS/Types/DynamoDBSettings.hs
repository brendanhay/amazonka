-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DynamoDBSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DynamoDBSettings
  ( DynamoDBSettings (..),

    -- * Smart constructor
    mkDynamoDBSettings,

    -- * Lenses
    ddsServiceAccessRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role used to define an Amazon DynamoDB target endpoint.
--
-- /See:/ 'mkDynamoDBSettings' smart constructor.
newtype DynamoDBSettings = DynamoDBSettings'
  { serviceAccessRoleARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DynamoDBSettings' with the minimum fields required to make a request.
--
-- * 'serviceAccessRoleARN' - The Amazon Resource Name (ARN) used by the service access IAM role.
mkDynamoDBSettings ::
  -- | 'serviceAccessRoleARN'
  Lude.Text ->
  DynamoDBSettings
mkDynamoDBSettings pServiceAccessRoleARN_ =
  DynamoDBSettings' {serviceAccessRoleARN = pServiceAccessRoleARN_}

-- | The Amazon Resource Name (ARN) used by the service access IAM role.
--
-- /Note:/ Consider using 'serviceAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsServiceAccessRoleARN :: Lens.Lens' DynamoDBSettings Lude.Text
ddsServiceAccessRoleARN = Lens.lens (serviceAccessRoleARN :: DynamoDBSettings -> Lude.Text) (\s a -> s {serviceAccessRoleARN = a} :: DynamoDBSettings)
{-# DEPRECATED ddsServiceAccessRoleARN "Use generic-lens or generic-optics with 'serviceAccessRoleARN' instead." #-}

instance Lude.FromJSON DynamoDBSettings where
  parseJSON =
    Lude.withObject
      "DynamoDBSettings"
      ( \x ->
          DynamoDBSettings' Lude.<$> (x Lude..: "ServiceAccessRoleArn")
      )

instance Lude.ToJSON DynamoDBSettings where
  toJSON DynamoDBSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ServiceAccessRoleArn" Lude..= serviceAccessRoleARN)]
      )
