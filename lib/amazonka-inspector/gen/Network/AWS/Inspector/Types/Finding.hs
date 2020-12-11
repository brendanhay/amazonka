-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Finding
  ( Finding (..),

    -- * Smart constructor
    mkFinding,

    -- * Lenses
    fService,
    fSeverity,
    fSchemaVersion,
    fConfidence,
    fAssetAttributes,
    fServiceAttributes,
    fId,
    fNumericSeverity,
    fAssetType,
    fTitle,
    fIndicatorOfCompromise,
    fDescription,
    fRecommendation,
    fArn,
    fAttributes,
    fUserAttributes,
    fCreatedAt,
    fUpdatedAt,
  )
where

import Network.AWS.Inspector.Types.AssetAttributes
import Network.AWS.Inspector.Types.AssetType
import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.InspectorServiceAttributes
import Network.AWS.Inspector.Types.Severity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an Amazon Inspector finding. This data type is used as the response element in the 'DescribeFindings' action.
--
-- /See:/ 'mkFinding' smart constructor.
data Finding = Finding'
  { service :: Lude.Maybe Lude.Text,
    severity :: Lude.Maybe Severity,
    schemaVersion :: Lude.Maybe Lude.Natural,
    confidence :: Lude.Maybe Lude.Natural,
    assetAttributes :: Lude.Maybe AssetAttributes,
    serviceAttributes :: Lude.Maybe InspectorServiceAttributes,
    id :: Lude.Maybe Lude.Text,
    numericSeverity :: Lude.Maybe Lude.Double,
    assetType :: Lude.Maybe AssetType,
    title :: Lude.Maybe Lude.Text,
    indicatorOfCompromise :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    recommendation :: Lude.Maybe Lude.Text,
    arn :: Lude.Text,
    attributes :: [Attribute],
    userAttributes :: [Attribute],
    createdAt :: Lude.Timestamp,
    updatedAt :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN that specifies the finding.
-- * 'assetAttributes' - A collection of attributes of the host from which the finding is generated.
-- * 'assetType' - The type of the host from which the finding is generated.
-- * 'attributes' - The system-defined attributes for the finding.
-- * 'confidence' - This data element is currently not used.
-- * 'createdAt' - The time when the finding was generated.
-- * 'description' - The description of the finding.
-- * 'id' - The ID of the finding.
-- * 'indicatorOfCompromise' - This data element is currently not used.
-- * 'numericSeverity' - The numeric value of the finding severity.
-- * 'recommendation' - The recommendation for the finding.
-- * 'schemaVersion' - The schema version of this data type.
-- * 'service' - The data element is set to "Inspector".
-- * 'serviceAttributes' - This data type is used in the 'Finding' data type.
-- * 'severity' - The finding severity. Values can be set to High, Medium, Low, and Informational.
-- * 'title' - The name of the finding.
-- * 'updatedAt' - The time when 'AddAttributesToFindings' is called.
-- * 'userAttributes' - The user-defined attributes that are assigned to the finding.
mkFinding ::
  -- | 'arn'
  Lude.Text ->
  -- | 'createdAt'
  Lude.Timestamp ->
  -- | 'updatedAt'
  Lude.Timestamp ->
  Finding
mkFinding pArn_ pCreatedAt_ pUpdatedAt_ =
  Finding'
    { service = Lude.Nothing,
      severity = Lude.Nothing,
      schemaVersion = Lude.Nothing,
      confidence = Lude.Nothing,
      assetAttributes = Lude.Nothing,
      serviceAttributes = Lude.Nothing,
      id = Lude.Nothing,
      numericSeverity = Lude.Nothing,
      assetType = Lude.Nothing,
      title = Lude.Nothing,
      indicatorOfCompromise = Lude.Nothing,
      description = Lude.Nothing,
      recommendation = Lude.Nothing,
      arn = pArn_,
      attributes = Lude.mempty,
      userAttributes = Lude.mempty,
      createdAt = pCreatedAt_,
      updatedAt = pUpdatedAt_
    }

-- | The data element is set to "Inspector".
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fService :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fService = Lens.lens (service :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {service = a} :: Finding)
{-# DEPRECATED fService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The finding severity. Values can be set to High, Medium, Low, and Informational.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSeverity :: Lens.Lens' Finding (Lude.Maybe Severity)
fSeverity = Lens.lens (severity :: Finding -> Lude.Maybe Severity) (\s a -> s {severity = a} :: Finding)
{-# DEPRECATED fSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The schema version of this data type.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSchemaVersion :: Lens.Lens' Finding (Lude.Maybe Lude.Natural)
fSchemaVersion = Lens.lens (schemaVersion :: Finding -> Lude.Maybe Lude.Natural) (\s a -> s {schemaVersion = a} :: Finding)
{-# DEPRECATED fSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | This data element is currently not used.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Finding (Lude.Maybe Lude.Natural)
fConfidence = Lens.lens (confidence :: Finding -> Lude.Maybe Lude.Natural) (\s a -> s {confidence = a} :: Finding)
{-# DEPRECATED fConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | A collection of attributes of the host from which the finding is generated.
--
-- /Note:/ Consider using 'assetAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAssetAttributes :: Lens.Lens' Finding (Lude.Maybe AssetAttributes)
fAssetAttributes = Lens.lens (assetAttributes :: Finding -> Lude.Maybe AssetAttributes) (\s a -> s {assetAttributes = a} :: Finding)
{-# DEPRECATED fAssetAttributes "Use generic-lens or generic-optics with 'assetAttributes' instead." #-}

-- | This data type is used in the 'Finding' data type.
--
-- /Note:/ Consider using 'serviceAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fServiceAttributes :: Lens.Lens' Finding (Lude.Maybe InspectorServiceAttributes)
fServiceAttributes = Lens.lens (serviceAttributes :: Finding -> Lude.Maybe InspectorServiceAttributes) (\s a -> s {serviceAttributes = a} :: Finding)
{-# DEPRECATED fServiceAttributes "Use generic-lens or generic-optics with 'serviceAttributes' instead." #-}

-- | The ID of the finding.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fId = Lens.lens (id :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Finding)
{-# DEPRECATED fId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The numeric value of the finding severity.
--
-- /Note:/ Consider using 'numericSeverity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fNumericSeverity :: Lens.Lens' Finding (Lude.Maybe Lude.Double)
fNumericSeverity = Lens.lens (numericSeverity :: Finding -> Lude.Maybe Lude.Double) (\s a -> s {numericSeverity = a} :: Finding)
{-# DEPRECATED fNumericSeverity "Use generic-lens or generic-optics with 'numericSeverity' instead." #-}

-- | The type of the host from which the finding is generated.
--
-- /Note:/ Consider using 'assetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAssetType :: Lens.Lens' Finding (Lude.Maybe AssetType)
fAssetType = Lens.lens (assetType :: Finding -> Lude.Maybe AssetType) (\s a -> s {assetType = a} :: Finding)
{-# DEPRECATED fAssetType "Use generic-lens or generic-optics with 'assetType' instead." #-}

-- | The name of the finding.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fTitle :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fTitle = Lens.lens (title :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: Finding)
{-# DEPRECATED fTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | This data element is currently not used.
--
-- /Note:/ Consider using 'indicatorOfCompromise' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fIndicatorOfCompromise :: Lens.Lens' Finding (Lude.Maybe Lude.Bool)
fIndicatorOfCompromise = Lens.lens (indicatorOfCompromise :: Finding -> Lude.Maybe Lude.Bool) (\s a -> s {indicatorOfCompromise = a} :: Finding)
{-# DEPRECATED fIndicatorOfCompromise "Use generic-lens or generic-optics with 'indicatorOfCompromise' instead." #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fDescription = Lens.lens (description :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Finding)
{-# DEPRECATED fDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The recommendation for the finding.
--
-- /Note:/ Consider using 'recommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRecommendation :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fRecommendation = Lens.lens (recommendation :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {recommendation = a} :: Finding)
{-# DEPRECATED fRecommendation "Use generic-lens or generic-optics with 'recommendation' instead." #-}

-- | The ARN that specifies the finding.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Finding Lude.Text
fArn = Lens.lens (arn :: Finding -> Lude.Text) (\s a -> s {arn = a} :: Finding)
{-# DEPRECATED fArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The system-defined attributes for the finding.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAttributes :: Lens.Lens' Finding [Attribute]
fAttributes = Lens.lens (attributes :: Finding -> [Attribute]) (\s a -> s {attributes = a} :: Finding)
{-# DEPRECATED fAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The user-defined attributes that are assigned to the finding.
--
-- /Note:/ Consider using 'userAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUserAttributes :: Lens.Lens' Finding [Attribute]
fUserAttributes = Lens.lens (userAttributes :: Finding -> [Attribute]) (\s a -> s {userAttributes = a} :: Finding)
{-# DEPRECATED fUserAttributes "Use generic-lens or generic-optics with 'userAttributes' instead." #-}

-- | The time when the finding was generated.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedAt :: Lens.Lens' Finding Lude.Timestamp
fCreatedAt = Lens.lens (createdAt :: Finding -> Lude.Timestamp) (\s a -> s {createdAt = a} :: Finding)
{-# DEPRECATED fCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The time when 'AddAttributesToFindings' is called.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUpdatedAt :: Lens.Lens' Finding Lude.Timestamp
fUpdatedAt = Lens.lens (updatedAt :: Finding -> Lude.Timestamp) (\s a -> s {updatedAt = a} :: Finding)
{-# DEPRECATED fUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Lude.FromJSON Finding where
  parseJSON =
    Lude.withObject
      "Finding"
      ( \x ->
          Finding'
            Lude.<$> (x Lude..:? "service")
            Lude.<*> (x Lude..:? "severity")
            Lude.<*> (x Lude..:? "schemaVersion")
            Lude.<*> (x Lude..:? "confidence")
            Lude.<*> (x Lude..:? "assetAttributes")
            Lude.<*> (x Lude..:? "serviceAttributes")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "numericSeverity")
            Lude.<*> (x Lude..:? "assetType")
            Lude.<*> (x Lude..:? "title")
            Lude.<*> (x Lude..:? "indicatorOfCompromise")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "recommendation")
            Lude.<*> (x Lude..: "arn")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "userAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "createdAt")
            Lude.<*> (x Lude..: "updatedAt")
      )
