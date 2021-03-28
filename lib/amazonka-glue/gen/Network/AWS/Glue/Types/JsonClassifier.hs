{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JsonClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.JsonClassifier
  ( JsonClassifier (..)
  -- * Smart constructor
  , mkJsonClassifier
  -- * Lenses
  , jcName
  , jcJsonPath
  , jcCreationTime
  , jcLastUpdated
  , jcVersion
  ) where

import qualified Network.AWS.Glue.Types.JsonPath as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A classifier for @JSON@ content.
--
-- /See:/ 'mkJsonClassifier' smart constructor.
data JsonClassifier = JsonClassifier'
  { name :: Types.Name
    -- ^ The name of the classifier.
  , jsonPath :: Types.JsonPath
    -- ^ A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that this classifier was registered.
  , lastUpdated :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that this classifier was last updated.
  , version :: Core.Maybe Core.Integer
    -- ^ The version of this classifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JsonClassifier' value with any optional fields omitted.
mkJsonClassifier
    :: Types.Name -- ^ 'name'
    -> Types.JsonPath -- ^ 'jsonPath'
    -> JsonClassifier
mkJsonClassifier name jsonPath
  = JsonClassifier'{name, jsonPath, creationTime = Core.Nothing,
                    lastUpdated = Core.Nothing, version = Core.Nothing}

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcName :: Lens.Lens' JsonClassifier Types.Name
jcName = Lens.field @"name"
{-# INLINEABLE jcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A @JsonPath@ string defining the JSON data for the classifier to classify. AWS Glue supports a subset of JsonPath, as described in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html#custom-classifier-json Writing JsonPath Custom Classifiers> .
--
-- /Note:/ Consider using 'jsonPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcJsonPath :: Lens.Lens' JsonClassifier Types.JsonPath
jcJsonPath = Lens.field @"jsonPath"
{-# INLINEABLE jcJsonPath #-}
{-# DEPRECATED jsonPath "Use generic-lens or generic-optics with 'jsonPath' instead"  #-}

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcCreationTime :: Lens.Lens' JsonClassifier (Core.Maybe Core.NominalDiffTime)
jcCreationTime = Lens.field @"creationTime"
{-# INLINEABLE jcCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcLastUpdated :: Lens.Lens' JsonClassifier (Core.Maybe Core.NominalDiffTime)
jcLastUpdated = Lens.field @"lastUpdated"
{-# INLINEABLE jcLastUpdated #-}
{-# DEPRECATED lastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead"  #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jcVersion :: Lens.Lens' JsonClassifier (Core.Maybe Core.Integer)
jcVersion = Lens.field @"version"
{-# INLINEABLE jcVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON JsonClassifier where
        parseJSON
          = Core.withObject "JsonClassifier" Core.$
              \ x ->
                JsonClassifier' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "JsonPath" Core.<*>
                    x Core..:? "CreationTime"
                    Core.<*> x Core..:? "LastUpdated"
                    Core.<*> x Core..:? "Version"
