{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.SourceAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.SourceAccessConfiguration
  ( SourceAccessConfiguration (..)
  -- * Smart constructor
  , mkSourceAccessConfiguration
  -- * Lenses
  , sacType
  , sacURI
  ) where

import qualified Network.AWS.Lambda.Types.Arn as Types
import qualified Network.AWS.Lambda.Types.SourceAccessType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@ 
--
-- /See:/ 'mkSourceAccessConfiguration' smart constructor.
data SourceAccessConfiguration = SourceAccessConfiguration'
  { type' :: Core.Maybe Types.SourceAccessType
    -- ^ To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
  , uri :: Core.Maybe Types.Arn
    -- ^ To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SourceAccessConfiguration' value with any optional fields omitted.
mkSourceAccessConfiguration
    :: SourceAccessConfiguration
mkSourceAccessConfiguration
  = SourceAccessConfiguration'{type' = Core.Nothing,
                               uri = Core.Nothing}

-- | To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacType :: Lens.Lens' SourceAccessConfiguration (Core.Maybe Types.SourceAccessType)
sacType = Lens.field @"type'"
{-# INLINEABLE sacType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
--
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sacURI :: Lens.Lens' SourceAccessConfiguration (Core.Maybe Types.Arn)
sacURI = Lens.field @"uri"
{-# INLINEABLE sacURI #-}
{-# DEPRECATED uri "Use generic-lens or generic-optics with 'uri' instead"  #-}

instance Core.FromJSON SourceAccessConfiguration where
        toJSON SourceAccessConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("Type" Core..=) Core.<$> type', ("URI" Core..=) Core.<$> uri])

instance Core.FromJSON SourceAccessConfiguration where
        parseJSON
          = Core.withObject "SourceAccessConfiguration" Core.$
              \ x ->
                SourceAccessConfiguration' Core.<$>
                  (x Core..:? "Type") Core.<*> x Core..:? "URI"
