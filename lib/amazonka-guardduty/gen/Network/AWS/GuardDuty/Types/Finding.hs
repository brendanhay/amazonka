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
    fARN,
    fService,
    fCreatedAt,
    fSeverity,
    fSchemaVersion,
    fAccountId,
    fConfidence,
    fResource,
    fPartition,
    fId,
    fTitle,
    fRegion,
    fType,
    fUpdatedAt,
    fDescription,
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
  { -- | The ARN of the finding.
    arn :: Lude.Text,
    service :: Lude.Maybe ServiceInfo,
    -- | The time and date when the finding was created.
    createdAt :: Lude.Text,
    -- | The severity of the finding.
    severity :: Lude.Double,
    -- | The version of the schema used for the finding.
    schemaVersion :: Lude.Text,
    -- | The ID of the account in which the finding was generated.
    accountId :: Lude.Text,
    -- | The confidence score for the finding.
    confidence :: Lude.Maybe Lude.Double,
    resource :: Resource,
    -- | The partition associated with the finding.
    partition :: Lude.Maybe Lude.Text,
    -- | The ID of the finding.
    id :: Lude.Text,
    -- | The title of the finding.
    title :: Lude.Maybe Lude.Text,
    -- | The Region where the finding was generated.
    region :: Lude.Text,
    -- | The type of finding.
    type' :: Lude.Text,
    -- | The time and date when the finding was last updated.
    updatedAt :: Lude.Text,
    -- | The description of the finding.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Finding' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the finding.
-- * 'service' -
-- * 'createdAt' - The time and date when the finding was created.
-- * 'severity' - The severity of the finding.
-- * 'schemaVersion' - The version of the schema used for the finding.
-- * 'accountId' - The ID of the account in which the finding was generated.
-- * 'confidence' - The confidence score for the finding.
-- * 'resource' -
-- * 'partition' - The partition associated with the finding.
-- * 'id' - The ID of the finding.
-- * 'title' - The title of the finding.
-- * 'region' - The Region where the finding was generated.
-- * 'type'' - The type of finding.
-- * 'updatedAt' - The time and date when the finding was last updated.
-- * 'description' - The description of the finding.
mkFinding ::
  -- | 'arn'
  Lude.Text ->
  -- | 'createdAt'
  Lude.Text ->
  -- | 'severity'
  Lude.Double ->
  -- | 'schemaVersion'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'resource'
  Resource ->
  -- | 'id'
  Lude.Text ->
  -- | 'region'
  Lude.Text ->
  -- | 'type''
  Lude.Text ->
  -- | 'updatedAt'
  Lude.Text ->
  Finding
mkFinding
  pARN_
  pCreatedAt_
  pSeverity_
  pSchemaVersion_
  pAccountId_
  pResource_
  pId_
  pRegion_
  pType_
  pUpdatedAt_ =
    Finding'
      { arn = pARN_,
        service = Lude.Nothing,
        createdAt = pCreatedAt_,
        severity = pSeverity_,
        schemaVersion = pSchemaVersion_,
        accountId = pAccountId_,
        confidence = Lude.Nothing,
        resource = pResource_,
        partition = Lude.Nothing,
        id = pId_,
        title = Lude.Nothing,
        region = pRegion_,
        type' = pType_,
        updatedAt = pUpdatedAt_,
        description = Lude.Nothing
      }

-- | The ARN of the finding.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fARN :: Lens.Lens' Finding Lude.Text
fARN = Lens.lens (arn :: Finding -> Lude.Text) (\s a -> s {arn = a} :: Finding)
{-# DEPRECATED fARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fService :: Lens.Lens' Finding (Lude.Maybe ServiceInfo)
fService = Lens.lens (service :: Finding -> Lude.Maybe ServiceInfo) (\s a -> s {service = a} :: Finding)
{-# DEPRECATED fService "Use generic-lens or generic-optics with 'service' instead." #-}

-- | The time and date when the finding was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCreatedAt :: Lens.Lens' Finding Lude.Text
fCreatedAt = Lens.lens (createdAt :: Finding -> Lude.Text) (\s a -> s {createdAt = a} :: Finding)
{-# DEPRECATED fCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The severity of the finding.
--
-- /Note:/ Consider using 'severity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSeverity :: Lens.Lens' Finding Lude.Double
fSeverity = Lens.lens (severity :: Finding -> Lude.Double) (\s a -> s {severity = a} :: Finding)
{-# DEPRECATED fSeverity "Use generic-lens or generic-optics with 'severity' instead." #-}

-- | The version of the schema used for the finding.
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fSchemaVersion :: Lens.Lens' Finding Lude.Text
fSchemaVersion = Lens.lens (schemaVersion :: Finding -> Lude.Text) (\s a -> s {schemaVersion = a} :: Finding)
{-# DEPRECATED fSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The ID of the account in which the finding was generated.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fAccountId :: Lens.Lens' Finding Lude.Text
fAccountId = Lens.lens (accountId :: Finding -> Lude.Text) (\s a -> s {accountId = a} :: Finding)
{-# DEPRECATED fAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The confidence score for the finding.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fConfidence :: Lens.Lens' Finding (Lude.Maybe Lude.Double)
fConfidence = Lens.lens (confidence :: Finding -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Finding)
{-# DEPRECATED fConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fResource :: Lens.Lens' Finding Resource
fResource = Lens.lens (resource :: Finding -> Resource) (\s a -> s {resource = a} :: Finding)
{-# DEPRECATED fResource "Use generic-lens or generic-optics with 'resource' instead." #-}

-- | The partition associated with the finding.
--
-- /Note:/ Consider using 'partition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fPartition :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fPartition = Lens.lens (partition :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {partition = a} :: Finding)
{-# DEPRECATED fPartition "Use generic-lens or generic-optics with 'partition' instead." #-}

-- | The ID of the finding.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fId :: Lens.Lens' Finding Lude.Text
fId = Lens.lens (id :: Finding -> Lude.Text) (\s a -> s {id = a} :: Finding)
{-# DEPRECATED fId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The title of the finding.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fTitle :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fTitle = Lens.lens (title :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: Finding)
{-# DEPRECATED fTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The Region where the finding was generated.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fRegion :: Lens.Lens' Finding Lude.Text
fRegion = Lens.lens (region :: Finding -> Lude.Text) (\s a -> s {region = a} :: Finding)
{-# DEPRECATED fRegion "Use generic-lens or generic-optics with 'region' instead." #-}

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

-- | The description of the finding.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDescription :: Lens.Lens' Finding (Lude.Maybe Lude.Text)
fDescription = Lens.lens (description :: Finding -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Finding)
{-# DEPRECATED fDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Finding where
  parseJSON =
    Lude.withObject
      "Finding"
      ( \x ->
          Finding'
            Lude.<$> (x Lude..: "arn")
            Lude.<*> (x Lude..:? "service")
            Lude.<*> (x Lude..: "createdAt")
            Lude.<*> (x Lude..: "severity")
            Lude.<*> (x Lude..: "schemaVersion")
            Lude.<*> (x Lude..: "accountId")
            Lude.<*> (x Lude..:? "confidence")
            Lude.<*> (x Lude..: "resource")
            Lude.<*> (x Lude..:? "partition")
            Lude.<*> (x Lude..: "id")
            Lude.<*> (x Lude..:? "title")
            Lude.<*> (x Lude..: "region")
            Lude.<*> (x Lude..: "type")
            Lude.<*> (x Lude..: "updatedAt")
            Lude.<*> (x Lude..:? "description")
      )
