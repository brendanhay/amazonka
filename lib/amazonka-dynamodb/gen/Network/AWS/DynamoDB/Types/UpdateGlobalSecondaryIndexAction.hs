{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.UpdateGlobalSecondaryIndexAction
  ( UpdateGlobalSecondaryIndexAction (..),

    -- * Smart constructor
    mkUpdateGlobalSecondaryIndexAction,

    -- * Lenses
    ugsiaIndexName,
    ugsiaProvisionedThroughput,
  )
where

import Network.AWS.DynamoDB.Types.ProvisionedThroughput
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the new provisioned throughput settings to be applied to a global secondary index.
--
-- /See:/ 'mkUpdateGlobalSecondaryIndexAction' smart constructor.
data UpdateGlobalSecondaryIndexAction = UpdateGlobalSecondaryIndexAction'
  { indexName ::
      Lude.Text,
    provisionedThroughput ::
      ProvisionedThroughput
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateGlobalSecondaryIndexAction' with the minimum fields required to make a request.
--
-- * 'indexName' - The name of the global secondary index to be updated.
-- * 'provisionedThroughput' - Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
mkUpdateGlobalSecondaryIndexAction ::
  -- | 'indexName'
  Lude.Text ->
  -- | 'provisionedThroughput'
  ProvisionedThroughput ->
  UpdateGlobalSecondaryIndexAction
mkUpdateGlobalSecondaryIndexAction
  pIndexName_
  pProvisionedThroughput_ =
    UpdateGlobalSecondaryIndexAction'
      { indexName = pIndexName_,
        provisionedThroughput = pProvisionedThroughput_
      }

-- | The name of the global secondary index to be updated.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsiaIndexName :: Lens.Lens' UpdateGlobalSecondaryIndexAction Lude.Text
ugsiaIndexName = Lens.lens (indexName :: UpdateGlobalSecondaryIndexAction -> Lude.Text) (\s a -> s {indexName = a} :: UpdateGlobalSecondaryIndexAction)
{-# DEPRECATED ugsiaIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | Represents the provisioned throughput settings for the specified global secondary index.
--
-- For current minimum and maximum provisioned throughput values, see <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Limits.html Service, Account, and Table Quotas> in the /Amazon DynamoDB Developer Guide/ .
--
-- /Note:/ Consider using 'provisionedThroughput' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugsiaProvisionedThroughput :: Lens.Lens' UpdateGlobalSecondaryIndexAction ProvisionedThroughput
ugsiaProvisionedThroughput = Lens.lens (provisionedThroughput :: UpdateGlobalSecondaryIndexAction -> ProvisionedThroughput) (\s a -> s {provisionedThroughput = a} :: UpdateGlobalSecondaryIndexAction)
{-# DEPRECATED ugsiaProvisionedThroughput "Use generic-lens or generic-optics with 'provisionedThroughput' instead." #-}

instance Lude.ToJSON UpdateGlobalSecondaryIndexAction where
  toJSON UpdateGlobalSecondaryIndexAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IndexName" Lude..= indexName),
            Lude.Just ("ProvisionedThroughput" Lude..= provisionedThroughput)
          ]
      )
