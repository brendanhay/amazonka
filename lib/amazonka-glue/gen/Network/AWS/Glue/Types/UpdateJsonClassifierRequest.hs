{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateJsonClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.UpdateJsonClassifierRequest
  ( UpdateJsonClassifierRequest (..)
  -- * Smart constructor
  , mkUpdateJsonClassifierRequest
  -- * Lenses
  , ujcrName
  , ujcrJsonPath
  ) where

import qualified Network.AWS.Glue.Types.JsonPath as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a JSON classifier to be updated.
--
-- /See:/ 'mkUpdateJsonClassifierRequest' smart constructor.
data UpdateJsonClassifierRequest = UpdateJsonClassifierRequest'
  { name :: Types.Name
    -- ^ The name of the classifier.
  , jsonPath :: Core.Maybe Types.JsonPath
    -- ^ A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJsonClassifierRequest' value with any optional fields omitted.
mkUpdateJsonClassifierRequest
    :: Types.Name -- ^ 'name'
    -> UpdateJsonClassifierRequest
mkUpdateJsonClassifierRequest name
  = UpdateJsonClassifierRequest'{name, jsonPath = Core.Nothing}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujcrName :: Lens.Lens' UpdateJsonClassifierRequest Types.Name
ujcrName = Lens.field @"name"
{-# INLINEABLE ujcrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujcrJsonPath :: Lens.Lens' UpdateJsonClassifierRequest (Core.Maybe Types.JsonPath)
ujcrJsonPath = Lens.field @"jsonPath"
{-# INLINEABLE ujcrJsonPath #-}
{-# DEPRECATED jsonPath "Use generic-lens or generic-optics with 'jsonPath' instead"  #-}

instance Core.FromJSON UpdateJsonClassifierRequest where
        toJSON UpdateJsonClassifierRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("JsonPath" Core..=) Core.<$> jsonPath])
