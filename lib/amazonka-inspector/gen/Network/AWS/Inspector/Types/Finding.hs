{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Finding
  ( Finding (..)
  -- * Smart constructor
  , mkFinding
  -- * Lenses
  , fArn
  , fAttributes
  , fUserAttributes
  , fCreatedAt
  , fUpdatedAt
  , fAssetAttributes
  , fAssetType
  , fConfidence
  , fDescription
  , fId
  , fIndicatorOfCompromise
  , fNumericSeverity
  , fRecommendation
  , fSchemaVersion
  , fService
  , fServiceAttributes
  , fSeverity
  , fTitle
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.AssetAttributes as Types
import qualified Network.AWS.Inspector.Types.AssetType as Types
import qualified Network.AWS.Inspector.Types.Attribute as Types
import qualified Network.AWS.Inspector.Types.Id as Types
import qualified Network.AWS.Inspector.Types.InspectorServiceAttributes as Types
import qualified Network.AWS.Inspector.Types.Service as Types
import qualified Network.AWS.Inspector.Types.Severity as Types
import qualified Network.AWS.Inspector.Types.Text as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an Amazon Inspector finding. This data type is used as the response element in the 'DescribeFindings' action.
--
-- /See:/ 'mkFinding' smart constructor.
data Finding = Finding'
  { arn :: Types.Arn
    -- ^ The ARN that specifies the finding.
  , attributes :: [Types.Attribute]
    -- ^ The system-defined attributes for the finding.
  , userAttributes :: [Types.Attribute]
    -- ^ The user-defined attributes that are assigned to the finding.
  , createdAt :: Core.NominalDiffTime
    -- ^ The time when the finding was generated.
  , updatedAt :: Core.NominalDiffTime
    -- ^ The time when 'AddAttributesToFindings' is called.
  , assetAttributes :: Core.Maybe Types.AssetAttributes
    -- ^ A collection of attributes of the host from which the finding is generated.
  , assetType :: Core.Maybe Types.AssetType
    -- ^ The type of the host from which the finding is generated.
  , confidence :: Core.Maybe Core.Natural
    -- ^ This data element is currently not used.
  , description :: Core.Maybe Types.Text
    -- ^ The description of the finding.
  , id :: Core.Maybe Types.Id
    -- ^ The ID of the finding.
  , indicatorOfCompromise :: Core.Maybe Core.Bool
    -- ^ This data element is currently not used.
  , numericSeverity :: Core.Maybe Core.Double
    -- ^ The numeric value of the finding severity.
  , recommendation :: Core.Maybe Types.Text
    -- ^ The recommendation for the finding.
  , schemaVersion :: Core.Maybe Core.Natural
    -- ^ The schema version of this data type.
  , service :: Core.Maybe Types.Service
    -- ^ The data element is set to "Inspector".
  , serviceAttributes :: Core.Maybe Types.InspectorServiceAttributes
    -- ^ This data type is used in the 'Finding' data type.
  , severity :: Core.Maybe Types.Severity
    -- ^ The finding severity. Values can be set to High, Medium, Low, and Informational.
  , title :: Core.Maybe Types.Text
    -- ^ The name of the finding.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Finding' value with any optional fields omitted.
mkFinding
    :: Types.Arn -- ^ 'arn'
    -> Core.NominalDiffTime -- ^ 'createdAt'
    -> Core.NominalDiffTime -- ^ 'updatedAt'
    -> Finding
mkFinding arn createdAt updatedAt
  = Finding'{arn, attributes = Core.mempty,
             userAttributes = Core.mempty, createdAt, updatedAt,
             assetAttributes = Core.Nothing, assetType = Core.Nothing,
             confidence = Core.Nothing, description = Core.Nothing,
             id = Core.Nothing, indicatorOfCompromise = Core.Nothing,
             numericSeverity = Core.Nothing, recommendation = Core.Nothing,
             schemaVersion = Core.Nothing, service = Core.Nothing,
             serviceAttributes = Core.Nothing, severity = Core.Nothing,
             title = Core.Nothing}

-- | The ARN that specifies the finding.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Finding Types.Arn
fArn = Lens.field @"arn"
{-# INLINEABLE fArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The system-defined attributes for the finding.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAttributes :: Lens.Lens' Finding [Types.Attribute]
fAttributes = Lens.field @"attributes"
{-# INLINEABLE fAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The user-defined attributes that are assigned to the finding.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUserAttributes :: Lens.Lens' Finding [Types.Attribute]
fUserAttributes = Lens.field @"userAttributes"
{-# INLINEABLE fUserAttributes #-}
{-# DEPRECATED userAttributes "Use generic-lens or generic-optics with 'userAttributes' instead"  #-}

-- | The time when the finding was generated.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedAt :: Lens.Lens' Finding Core.NominalDiffTime
fCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE fCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The time when 'AddAttributesToFindings' is called.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUpdatedAt :: Lens.Lens' Finding Core.NominalDiffTime
fUpdatedAt = Lens.field @"updatedAt"
{-# INLINEABLE fUpdatedAt #-}
{-# DEPRECATED updatedAt "Use generic-lens or generic-optics with 'updatedAt' instead"  #-}

-- | A collection of attributes of the host from which the finding is generated.
--
-- /Note:/ Consider using 'assetAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAssetAttributes :: Lens.Lens' Finding (Core.Maybe Types.AssetAttributes)
fAssetAttributes = Lens.field @"assetAttributes"
{-# INLINEABLE fAssetAttributes #-}
{-# DEPRECATED assetAttributes "Use generic-lens or generic-optics with 'assetAttributes' instead"  #-}

-- | The type of the host from which the finding is generated.
--
-- /Note:/ Consider using 'assetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAssetType :: Lens.Lens' Finding (Core.Maybe Types.AssetType)
fAssetType = Lens.field @"assetType"
{-# INLINEABLE fAssetType #-}
{-# DEPRECATED assetType "Use generic-lens or generic-optics with 'assetType' instead"  #-}

-- | This data element is currently not used.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Finding (Core.Maybe Core.Natural)
fConfidence = Lens.field @"confidence"
{-# INLINEABLE fConfidence #-}
{-# DEPRECATED confidence "Use generic-lens or generic-optics with 'confidence' instead"  #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Finding (Core.Maybe Types.Text)
fDescription = Lens.field @"description"
{-# INLINEABLE fDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the finding.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Finding (Core.Maybe Types.Id)
fId = Lens.field @"id"
{-# INLINEABLE fId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | This data element is currently not used.
--
-- /Note:/ Consider using 'indicatorOfCompromise' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fIndicatorOfCompromise :: Lens.Lens' Finding (Core.Maybe Core.Bool)
fIndicatorOfCompromise = Lens.field @"indicatorOfCompromise"
{-# INLINEABLE fIndicatorOfCompromise #-}
{-# DEPRECATED indicatorOfCompromise "Use generic-lens or generic-optics with 'indicatorOfCompromise' instead"  #-}

-- | The numeric value of the finding severity.
--
-- /Note:/ Consider using 'numericSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fNumericSeverity :: Lens.Lens' Finding (Core.Maybe Core.Double)
fNumericSeverity = Lens.field @"numericSeverity"
{-# INLINEABLE fNumericSeverity #-}
{-# DEPRECATED numericSeverity "Use generic-lens or generic-optics with 'numericSeverity' instead"  #-}

-- | The recommendation for the finding.
--
-- /Note:/ Consider using 'recommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRecommendation :: Lens.Lens' Finding (Core.Maybe Types.Text)
fRecommendation = Lens.field @"recommendation"
{-# INLINEABLE fRecommendation #-}
{-# DEPRECATED recommendation "Use generic-lens or generic-optics with 'recommendation' instead"  #-}

-- | The schema version of this data type.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSchemaVersion :: Lens.Lens' Finding (Core.Maybe Core.Natural)
fSchemaVersion = Lens.field @"schemaVersion"
{-# INLINEABLE fSchemaVersion #-}
{-# DEPRECATED schemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead"  #-}

-- | The data element is set to "Inspector".
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fService :: Lens.Lens' Finding (Core.Maybe Types.Service)
fService = Lens.field @"service"
{-# INLINEABLE fService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | This data type is used in the 'Finding' data type.
--
-- /Note:/ Consider using 'serviceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fServiceAttributes :: Lens.Lens' Finding (Core.Maybe Types.InspectorServiceAttributes)
fServiceAttributes = Lens.field @"serviceAttributes"
{-# INLINEABLE fServiceAttributes #-}
{-# DEPRECATED serviceAttributes "Use generic-lens or generic-optics with 'serviceAttributes' instead"  #-}

-- | The finding severity. Values can be set to High, Medium, Low, and Informational.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSeverity :: Lens.Lens' Finding (Core.Maybe Types.Severity)
fSeverity = Lens.field @"severity"
{-# INLINEABLE fSeverity #-}
{-# DEPRECATED severity "Use generic-lens or generic-optics with 'severity' instead"  #-}

-- | The name of the finding.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fTitle :: Lens.Lens' Finding (Core.Maybe Types.Text)
fTitle = Lens.field @"title"
{-# INLINEABLE fTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

instance Core.FromJSON Finding where
        parseJSON
          = Core.withObject "Finding" Core.$
              \ x ->
                Finding' Core.<$>
                  (x Core..: "arn") Core.<*>
                    x Core..:? "attributes" Core..!= Core.mempty
                    Core.<*> x Core..:? "userAttributes" Core..!= Core.mempty
                    Core.<*> x Core..: "createdAt"
                    Core.<*> x Core..: "updatedAt"
                    Core.<*> x Core..:? "assetAttributes"
                    Core.<*> x Core..:? "assetType"
                    Core.<*> x Core..:? "confidence"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "indicatorOfCompromise"
                    Core.<*> x Core..:? "numericSeverity"
                    Core.<*> x Core..:? "recommendation"
                    Core.<*> x Core..:? "schemaVersion"
                    Core.<*> x Core..:? "service"
                    Core.<*> x Core..:? "serviceAttributes"
                    Core.<*> x Core..:? "severity"
                    Core.<*> x Core..:? "title"
