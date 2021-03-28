{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ThirdPartyJobDetails
  ( ThirdPartyJobDetails (..)
  -- * Smart constructor
  , mkThirdPartyJobDetails
  -- * Lenses
  , tpjdData
  , tpjdId
  , tpjdNonce
  ) where

import qualified Network.AWS.CodePipeline.Types.Nonce as Types
import qualified Network.AWS.CodePipeline.Types.ThirdPartyJobData as Types
import qualified Network.AWS.CodePipeline.Types.ThirdPartyJobId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of a job sent in response to a @GetThirdPartyJobDetails@ request.
--
-- /See:/ 'mkThirdPartyJobDetails' smart constructor.
data ThirdPartyJobDetails = ThirdPartyJobDetails'
  { data' :: Core.Maybe Types.ThirdPartyJobData
    -- ^ The data to be returned by the third party job worker.
  , id :: Core.Maybe Types.ThirdPartyJobId
    -- ^ The identifier used to identify the job details in AWS CodePipeline.
  , nonce :: Core.Maybe Types.Nonce
    -- ^ A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeThirdPartyJob' request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ThirdPartyJobDetails' value with any optional fields omitted.
mkThirdPartyJobDetails
    :: ThirdPartyJobDetails
mkThirdPartyJobDetails
  = ThirdPartyJobDetails'{data' = Core.Nothing, id = Core.Nothing,
                          nonce = Core.Nothing}

-- | The data to be returned by the third party job worker.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjdData :: Lens.Lens' ThirdPartyJobDetails (Core.Maybe Types.ThirdPartyJobData)
tpjdData = Lens.field @"data'"
{-# INLINEABLE tpjdData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The identifier used to identify the job details in AWS CodePipeline.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjdId :: Lens.Lens' ThirdPartyJobDetails (Core.Maybe Types.ThirdPartyJobId)
tpjdId = Lens.field @"id"
{-# INLINEABLE tpjdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A system-generated random number that AWS CodePipeline uses to ensure that the job is being worked on by only one job worker. Use this number in an 'AcknowledgeThirdPartyJob' request.
--
-- /Note:/ Consider using 'nonce' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tpjdNonce :: Lens.Lens' ThirdPartyJobDetails (Core.Maybe Types.Nonce)
tpjdNonce = Lens.field @"nonce"
{-# INLINEABLE tpjdNonce #-}
{-# DEPRECATED nonce "Use generic-lens or generic-optics with 'nonce' instead"  #-}

instance Core.FromJSON ThirdPartyJobDetails where
        parseJSON
          = Core.withObject "ThirdPartyJobDetails" Core.$
              \ x ->
                ThirdPartyJobDetails' Core.<$>
                  (x Core..:? "data") Core.<*> x Core..:? "id" Core.<*>
                    x Core..:? "nonce"
