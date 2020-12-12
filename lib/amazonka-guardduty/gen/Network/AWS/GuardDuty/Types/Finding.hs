{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Finding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Finding
  ( Finding (..),

    -- * Smart constructor
    mkFinding,

    -- * Lenses
    fService,
    fConfidence,
    fPartition,
    fTitle,
    fDescription,
    fAccountId,
    fARN,
    fCreatedAt,
    fId,
    fRegion,
    fResource,
    fSchemaVersion,
    fSeverity,
    fType,
    fUpdatedAt,
  )
where

import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.ServiceInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the finding, which is generated when abnormal or suspicious activity is detected.
--
-- /See:/ 'mkFinding' smart constructor.
data Finding = Finding'
  { service :: Lude.Maybe ServiceInfo,
    confidence :: Lude.Maybe Lude.Double,
    partition :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    accountId :: Lude.Text,
    arn :: Lude.Text,
    createdAt :: Lude.Text,
    id :: Lude.Text,
    region :: Lude.Text,
    resource :: Resource,
    schemaVersion :: Lude.Text,
    severity :: Lude.Double,
    type' :: Lude.Text,
    updatedAt :: Lude.Text
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
-- * 'accountId' - The ID of the account in which the finding was generated.
-- * 'arn' - The ARN of the finding.
-- * 'confidence' - The confidence score for the finding.
-- * 'createdAt' - The time and date when the finding was created.
-- * 'description' - The description of the finding.
-- * 'id' - The ID of the finding.
-- * 'partition' - The partition associated with the finding.
-- * 'region' - The Region where the finding was generated.
-- * 'resource' - Undocumented field.
-- * 'schemaVersion' - The version of the schema used for the finding.
-- * 'service' - Undocumented field.
-- * 'severity' - The severity of the finding.
-- * 'title' - The title of the finding.
-- * 'type'' - The type of finding.
-- * 'updatedAt' - The time and date when the finding was last updated.
mkFinding ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'createdAt'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'region'
  Lude.Text ->
  -- | 'resource'
  Resource ->
  -- | 'schemaVersion'
  Lude.Text ->
  -- | 'severity'
  Lude.Double ->
  -- | 'type''
  Lude.Text ->
  -- | 'updatedAt'
  Lude.Text ->
  Finding
mkFinding
  pAccountId_
  pARN_
  pCreatedAt_
  pId_
  pRegion_
  pResource_
  pSchemaVersion_
  pSeverity_
  pType_
  pUpdatedAt_ =
    Finding'
      { service = Lude.Nothing,
        confidence = Lude.Nothing,
        partition = Lude.Nothing,
        title = Lude.Nothing,
        description = Lude.Nothing,
        accountId = pAccountId_,
        arn = pARN_,
        createdAt = pCreatedAt_,
        id = pId_,
        region = pRegion_,
        resource = pResource_,
        schemaVersion = pSchemaVersion_,
        severity = pSeverity_,
        type' = pType_,
        updatedAt = pUpdatedAt_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fService :: Lens.Lens' Finding (Lude.Maybe ServiceInfo)
fService = Lens.lens (service :: Finding -> Lude.Maybe ServiceInfo) (\s a -> s {service = a} :: Finding)
{-# DEPRECATED fService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The confidence score for the finding.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Finding (Lude.Maybe Lude.Double)
fConfidence = Lens.lens (confidence :: Finding -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Finding)
{-# DEPRECATED fConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The partition associated with the finding.
--
-- /Note:/ Consider using 'partition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fPartition :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fPartition = Lens.lens (partition :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {partition = a} :: Finding)
{-# DEPRECATED fPartition "Use generic-lens or generic-optics with 'partition' instead." #-}

-- | The title of the finding.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fTitle :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fTitle = Lens.lens (title :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: Finding)
{-# DEPRECATED fTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fDescription = Lens.lens (description :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Finding)
{-# DEPRECATED fDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the account in which the finding was generated.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAccountId :: Lens.Lens' Finding Lude.Text
fAccountId = Lens.lens (accountId :: Finding -> Lude.Text) (\s a -> s {accountId = a} :: Finding)
{-# DEPRECATED fAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The ARN of the finding.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fARN :: Lens.Lens' Finding Lude.Text
fARN = Lens.lens (arn :: Finding -> Lude.Text) (\s a -> s {arn = a} :: Finding)
{-# DEPRECATED fARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time and date when the finding was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedAt :: Lens.Lens' Finding Lude.Text
fCreatedAt = Lens.lens (createdAt :: Finding -> Lude.Text) (\s a -> s {createdAt = a} :: Finding)
{-# DEPRECATED fCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The ID of the finding.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Finding Lude.Text
fId = Lens.lens (id :: Finding -> Lude.Text) (\s a -> s {id = a} :: Finding)
{-# DEPRECATED fId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Region where the finding was generated.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRegion :: Lens.Lens' Finding Lude.Text
fRegion = Lens.lens (region :: Finding -> Lude.Text) (\s a -> s {region = a} :: Finding)
{-# DEPRECATED fRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fResource :: Lens.Lens' Finding Resource
fResource = Lens.lens (resource :: Finding -> Resource) (\s a -> s {resource = a} :: Finding)
{-# DEPRECATED fResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The version of the schema used for the finding.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSchemaVersion :: Lens.Lens' Finding Lude.Text
fSchemaVersion = Lens.lens (schemaVersion :: Finding -> Lude.Text) (\s a -> s {schemaVersion = a} :: Finding)
{-# DEPRECATED fSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The severity of the finding.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSeverity :: Lens.Lens' Finding Lude.Double
fSeverity = Lens.lens (severity :: Finding -> Lude.Double) (\s a -> s {severity = a} :: Finding)
{-# DEPRECATED fSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The type of finding.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fType :: Lens.Lens' Finding Lude.Text
fType = Lens.lens (type' :: Finding -> Lude.Text) (\s a -> s {type' = a} :: Finding)
{-# DEPRECATED fType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The time and date when the finding was last updated.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fUpdatedAt :: Lens.Lens' Finding Lude.Text
fUpdatedAt = Lens.lens (updatedAt :: Finding -> Lude.Text) (\s a -> s {updatedAt = a} :: Finding)
{-# DEPRECATED fUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Lude.FromJSON Finding where
  parseJSON =
    Lude.withObject
      "Finding"
      ( \x ->
          Finding'
            Lude.<$> (x Lude..:? "service")
            Lude.<*> (x Lude..:? "confidence")
            Lude.<*> (x Lude..:? "partition")
            Lude.<*> (x Lude..:? "title")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "accountId")
            Lude.<*> (x Lude..: "arn")
            Lude.<*> (x Lude..: "createdAt")
            Lude.<*> (x Lude..: "id")
            Lude.<*> (x Lude..: "region")
            Lude.<*> (x Lude..: "resource")
            Lude.<*> (x Lude..: "schemaVersion")
            Lude.<*> (x Lude..: "severity")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..: "updatedAt")
      )
