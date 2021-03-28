{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
  ( CognitoOptionsStatus (..)
  -- * Smart constructor
  , mkCognitoOptionsStatus
  -- * Lenses
  , cosOptions
  , cosStatus
  ) where

import qualified Network.AWS.ElasticSearch.Types.CognitoOptions as Types
import qualified Network.AWS.ElasticSearch.Types.OptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Status of the Cognito options for the specified Elasticsearch domain.
--
-- /See:/ 'mkCognitoOptionsStatus' smart constructor.
data CognitoOptionsStatus = CognitoOptionsStatus'
  { options :: Types.CognitoOptions
    -- ^ Specifies the Cognito options for the specified Elasticsearch domain.
  , status :: Types.OptionStatus
    -- ^ Specifies the status of the Cognito options for the specified Elasticsearch domain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CognitoOptionsStatus' value with any optional fields omitted.
mkCognitoOptionsStatus
    :: Types.CognitoOptions -- ^ 'options'
    -> Types.OptionStatus -- ^ 'status'
    -> CognitoOptionsStatus
mkCognitoOptionsStatus options status
  = CognitoOptionsStatus'{options, status}

-- | Specifies the Cognito options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosOptions :: Lens.Lens' CognitoOptionsStatus Types.CognitoOptions
cosOptions = Lens.field @"options"
{-# INLINEABLE cosOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | Specifies the status of the Cognito options for the specified Elasticsearch domain.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cosStatus :: Lens.Lens' CognitoOptionsStatus Types.OptionStatus
cosStatus = Lens.field @"status"
{-# INLINEABLE cosStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON CognitoOptionsStatus where
        parseJSON
          = Core.withObject "CognitoOptionsStatus" Core.$
              \ x ->
                CognitoOptionsStatus' Core.<$>
                  (x Core..: "Options") Core.<*> x Core..: "Status"
