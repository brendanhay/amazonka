-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
  ( RelationalDatabaseSnapshot (..),

    -- * Smart constructor
    mkRelationalDatabaseSnapshot,

    -- * Lenses
    rdsEngineVersion,
    rdsState,
    rdsFromRelationalDatabaseName,
    rdsResourceType,
    rdsFromRelationalDatabaseBlueprintId,
    rdsArn,
    rdsCreatedAt,
    rdsLocation,
    rdsEngine,
    rdsName,
    rdsSizeInGb,
    rdsSupportCode,
    rdsFromRelationalDatabaseARN,
    rdsFromRelationalDatabaseBundleId,
    rdsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes a database snapshot.
--
-- /See:/ 'mkRelationalDatabaseSnapshot' smart constructor.
data RelationalDatabaseSnapshot = RelationalDatabaseSnapshot'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    state :: Lude.Maybe Lude.Text,
    fromRelationalDatabaseName ::
      Lude.Maybe Lude.Text,
    resourceType ::
      Lude.Maybe ResourceType,
    fromRelationalDatabaseBlueprintId ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    createdAt ::
      Lude.Maybe Lude.Timestamp,
    location ::
      Lude.Maybe ResourceLocation,
    engine :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    sizeInGb :: Lude.Maybe Lude.Int,
    supportCode :: Lude.Maybe Lude.Text,
    fromRelationalDatabaseARN ::
      Lude.Maybe Lude.Text,
    fromRelationalDatabaseBundleId ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RelationalDatabaseSnapshot' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the database snapshot.
-- * 'createdAt' - The timestamp when the database snapshot was created.
-- * 'engine' - The software of the database snapshot (for example, @MySQL@ )
-- * 'engineVersion' - The database engine version for the database snapshot (for example, @5.7.23@ ).
-- * 'fromRelationalDatabaseARN' - The Amazon Resource Name (ARN) of the database from which the database snapshot was created.
-- * 'fromRelationalDatabaseBlueprintId' - The blueprint ID of the database from which the database snapshot was created. A blueprint describes the major engine version of a database.
-- * 'fromRelationalDatabaseBundleId' - The bundle ID of the database from which the database snapshot was created.
-- * 'fromRelationalDatabaseName' - The name of the source database from which the database snapshot was created.
-- * 'location' - The Region name and Availability Zone where the database snapshot is located.
-- * 'name' - The name of the database snapshot.
-- * 'resourceType' - The Lightsail resource type.
-- * 'sizeInGb' - The size of the disk in GB (for example, @32@ ) for the database snapshot.
-- * 'state' - The state of the database snapshot.
-- * 'supportCode' - The support code for the database snapshot. Include this code in your email to support when you have questions about a database snapshot in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkRelationalDatabaseSnapshot ::
  RelationalDatabaseSnapshot
mkRelationalDatabaseSnapshot =
  RelationalDatabaseSnapshot'
    { engineVersion = Lude.Nothing,
      state = Lude.Nothing,
      fromRelationalDatabaseName = Lude.Nothing,
      resourceType = Lude.Nothing,
      fromRelationalDatabaseBlueprintId = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      engine = Lude.Nothing,
      name = Lude.Nothing,
      sizeInGb = Lude.Nothing,
      supportCode = Lude.Nothing,
      fromRelationalDatabaseARN = Lude.Nothing,
      fromRelationalDatabaseBundleId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The database engine version for the database snapshot (for example, @5.7.23@ ).
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsEngineVersion :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsEngineVersion = Lens.lens (engineVersion :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The state of the database snapshot.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsState :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsState = Lens.lens (state :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The name of the source database from which the database snapshot was created.
--
-- /Note:/ Consider using 'fromRelationalDatabaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseName :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsFromRelationalDatabaseName = Lens.lens (fromRelationalDatabaseName :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromRelationalDatabaseName = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsFromRelationalDatabaseName "Use generic-lens or generic-optics with 'fromRelationalDatabaseName' instead." #-}

-- | The Lightsail resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsResourceType :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe ResourceType)
rdsResourceType = Lens.lens (resourceType :: RelationalDatabaseSnapshot -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The blueprint ID of the database from which the database snapshot was created. A blueprint describes the major engine version of a database.
--
-- /Note:/ Consider using 'fromRelationalDatabaseBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseBlueprintId :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsFromRelationalDatabaseBlueprintId = Lens.lens (fromRelationalDatabaseBlueprintId :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromRelationalDatabaseBlueprintId = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsFromRelationalDatabaseBlueprintId "Use generic-lens or generic-optics with 'fromRelationalDatabaseBlueprintId' instead." #-}

-- | The Amazon Resource Name (ARN) of the database snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsArn :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsArn = Lens.lens (arn :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the database snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsCreatedAt :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Timestamp)
rdsCreatedAt = Lens.lens (createdAt :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Region name and Availability Zone where the database snapshot is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsLocation :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe ResourceLocation)
rdsLocation = Lens.lens (location :: RelationalDatabaseSnapshot -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The software of the database snapshot (for example, @MySQL@ )
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsEngine :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsEngine = Lens.lens (engine :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The name of the database snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsName :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsName = Lens.lens (name :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of the disk in GB (for example, @32@ ) for the database snapshot.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsSizeInGb :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Int)
rdsSizeInGb = Lens.lens (sizeInGb :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The support code for the database snapshot. Include this code in your email to support when you have questions about a database snapshot in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsSupportCode :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsSupportCode = Lens.lens (supportCode :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The Amazon Resource Name (ARN) of the database from which the database snapshot was created.
--
-- /Note:/ Consider using 'fromRelationalDatabaseARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseARN :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsFromRelationalDatabaseARN = Lens.lens (fromRelationalDatabaseARN :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromRelationalDatabaseARN = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsFromRelationalDatabaseARN "Use generic-lens or generic-optics with 'fromRelationalDatabaseARN' instead." #-}

-- | The bundle ID of the database from which the database snapshot was created.
--
-- /Note:/ Consider using 'fromRelationalDatabaseBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsFromRelationalDatabaseBundleId :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe Lude.Text)
rdsFromRelationalDatabaseBundleId = Lens.lens (fromRelationalDatabaseBundleId :: RelationalDatabaseSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromRelationalDatabaseBundleId = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsFromRelationalDatabaseBundleId "Use generic-lens or generic-optics with 'fromRelationalDatabaseBundleId' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsTags :: Lens.Lens' RelationalDatabaseSnapshot (Lude.Maybe [Tag])
rdsTags = Lens.lens (tags :: RelationalDatabaseSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: RelationalDatabaseSnapshot)
{-# DEPRECATED rdsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON RelationalDatabaseSnapshot where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseSnapshot"
      ( \x ->
          RelationalDatabaseSnapshot'
            Lude.<$> (x Lude..:? "engineVersion")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "fromRelationalDatabaseName")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "fromRelationalDatabaseBlueprintId")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "engine")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "sizeInGb")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "fromRelationalDatabaseArn")
            Lude.<*> (x Lude..:? "fromRelationalDatabaseBundleId")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
