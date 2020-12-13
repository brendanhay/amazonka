{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpgradeAppliedSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a single directory in-place using the @PublishedSchemaArn@ with schema updates found in @MinorVersion@ . Backwards-compatible minor version upgrades are instantaneously available for readers on all objects in the directory. Note: This is a synchronous API call and upgrades only one schema on a given directory per call. To upgrade multiple directories from one schema, you would need to call this API on each directory.
module Network.AWS.CloudDirectory.UpgradeAppliedSchema
  ( -- * Creating a request
    UpgradeAppliedSchema (..),
    mkUpgradeAppliedSchema,

    -- ** Request lenses
    uasDirectoryARN,
    uasPublishedSchemaARN,
    uasDryRun,

    -- * Destructuring the response
    UpgradeAppliedSchemaResponse (..),
    mkUpgradeAppliedSchemaResponse,

    -- ** Response lenses
    uasrsDirectoryARN,
    uasrsUpgradedSchemaARN,
    uasrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpgradeAppliedSchema' smart constructor.
data UpgradeAppliedSchema = UpgradeAppliedSchema'
  { -- | The ARN for the directory to which the upgraded schema will be applied.
    directoryARN :: Lude.Text,
    -- | The revision of the published schema to upgrade the directory to.
    publishedSchemaARN :: Lude.Text,
    -- | Used for testing whether the major version schemas are backward compatible or not. If schema compatibility fails, an exception would be thrown else the call would succeed but no changes will be saved. This parameter is optional.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeAppliedSchema' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN for the directory to which the upgraded schema will be applied.
-- * 'publishedSchemaARN' - The revision of the published schema to upgrade the directory to.
-- * 'dryRun' - Used for testing whether the major version schemas are backward compatible or not. If schema compatibility fails, an exception would be thrown else the call would succeed but no changes will be saved. This parameter is optional.
mkUpgradeAppliedSchema ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'publishedSchemaARN'
  Lude.Text ->
  UpgradeAppliedSchema
mkUpgradeAppliedSchema pDirectoryARN_ pPublishedSchemaARN_ =
  UpgradeAppliedSchema'
    { directoryARN = pDirectoryARN_,
      publishedSchemaARN = pPublishedSchemaARN_,
      dryRun = Lude.Nothing
    }

-- | The ARN for the directory to which the upgraded schema will be applied.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasDirectoryARN :: Lens.Lens' UpgradeAppliedSchema Lude.Text
uasDirectoryARN = Lens.lens (directoryARN :: UpgradeAppliedSchema -> Lude.Text) (\s a -> s {directoryARN = a} :: UpgradeAppliedSchema)
{-# DEPRECATED uasDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The revision of the published schema to upgrade the directory to.
--
-- /Note:/ Consider using 'publishedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasPublishedSchemaARN :: Lens.Lens' UpgradeAppliedSchema Lude.Text
uasPublishedSchemaARN = Lens.lens (publishedSchemaARN :: UpgradeAppliedSchema -> Lude.Text) (\s a -> s {publishedSchemaARN = a} :: UpgradeAppliedSchema)
{-# DEPRECATED uasPublishedSchemaARN "Use generic-lens or generic-optics with 'publishedSchemaARN' instead." #-}

-- | Used for testing whether the major version schemas are backward compatible or not. If schema compatibility fails, an exception would be thrown else the call would succeed but no changes will be saved. This parameter is optional.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasDryRun :: Lens.Lens' UpgradeAppliedSchema (Lude.Maybe Lude.Bool)
uasDryRun = Lens.lens (dryRun :: UpgradeAppliedSchema -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: UpgradeAppliedSchema)
{-# DEPRECATED uasDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest UpgradeAppliedSchema where
  type Rs UpgradeAppliedSchema = UpgradeAppliedSchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpgradeAppliedSchemaResponse'
            Lude.<$> (x Lude..?> "DirectoryArn")
            Lude.<*> (x Lude..?> "UpgradedSchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpgradeAppliedSchema where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpgradeAppliedSchema where
  toJSON UpgradeAppliedSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DirectoryArn" Lude..= directoryARN),
            Lude.Just ("PublishedSchemaArn" Lude..= publishedSchemaARN),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath UpgradeAppliedSchema where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/schema/upgradeapplied"

instance Lude.ToQuery UpgradeAppliedSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpgradeAppliedSchemaResponse' smart constructor.
data UpgradeAppliedSchemaResponse = UpgradeAppliedSchemaResponse'
  { -- | The ARN of the directory that is returned as part of the response.
    directoryARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the upgraded schema that is returned as part of the response.
    upgradedSchemaARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradeAppliedSchemaResponse' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory that is returned as part of the response.
-- * 'upgradedSchemaARN' - The ARN of the upgraded schema that is returned as part of the response.
-- * 'responseStatus' - The response status code.
mkUpgradeAppliedSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpgradeAppliedSchemaResponse
mkUpgradeAppliedSchemaResponse pResponseStatus_ =
  UpgradeAppliedSchemaResponse'
    { directoryARN = Lude.Nothing,
      upgradedSchemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the directory that is returned as part of the response.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsDirectoryARN :: Lens.Lens' UpgradeAppliedSchemaResponse (Lude.Maybe Lude.Text)
uasrsDirectoryARN = Lens.lens (directoryARN :: UpgradeAppliedSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {directoryARN = a} :: UpgradeAppliedSchemaResponse)
{-# DEPRECATED uasrsDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The ARN of the upgraded schema that is returned as part of the response.
--
-- /Note:/ Consider using 'upgradedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsUpgradedSchemaARN :: Lens.Lens' UpgradeAppliedSchemaResponse (Lude.Maybe Lude.Text)
uasrsUpgradedSchemaARN = Lens.lens (upgradedSchemaARN :: UpgradeAppliedSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {upgradedSchemaARN = a} :: UpgradeAppliedSchemaResponse)
{-# DEPRECATED uasrsUpgradedSchemaARN "Use generic-lens or generic-optics with 'upgradedSchemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uasrsResponseStatus :: Lens.Lens' UpgradeAppliedSchemaResponse Lude.Int
uasrsResponseStatus = Lens.lens (responseStatus :: UpgradeAppliedSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpgradeAppliedSchemaResponse)
{-# DEPRECATED uasrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
