{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.UpgradePublishedSchema
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Upgrades a published schema under a new minor version revision using the current contents of @DevelopmentSchemaArn@ .
module Network.AWS.CloudDirectory.UpgradePublishedSchema
  ( -- * Creating a request
    UpgradePublishedSchema (..),
    mkUpgradePublishedSchema,

    -- ** Request lenses
    upsDryRun,
    upsDevelopmentSchemaARN,
    upsPublishedSchemaARN,
    upsMinorVersion,

    -- * Destructuring the response
    UpgradePublishedSchemaResponse (..),
    mkUpgradePublishedSchemaResponse,

    -- ** Response lenses
    upsrsUpgradedSchemaARN,
    upsrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpgradePublishedSchema' smart constructor.
data UpgradePublishedSchema = UpgradePublishedSchema'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    developmentSchemaARN :: Lude.Text,
    publishedSchemaARN :: Lude.Text,
    minorVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradePublishedSchema' with the minimum fields required to make a request.
--
-- * 'developmentSchemaARN' - The ARN of the development schema with the changes used for the upgrade.
-- * 'dryRun' - Used for testing whether the Development schema provided is backwards compatible, or not, with the publish schema provided by the user to be upgraded. If schema compatibility fails, an exception would be thrown else the call would succeed. This parameter is optional and defaults to false.
-- * 'minorVersion' - Identifies the minor version of the published schema that will be created. This parameter is NOT optional.
-- * 'publishedSchemaARN' - The ARN of the published schema to be upgraded.
mkUpgradePublishedSchema ::
  -- | 'developmentSchemaARN'
  Lude.Text ->
  -- | 'publishedSchemaARN'
  Lude.Text ->
  -- | 'minorVersion'
  Lude.Text ->
  UpgradePublishedSchema
mkUpgradePublishedSchema
  pDevelopmentSchemaARN_
  pPublishedSchemaARN_
  pMinorVersion_ =
    UpgradePublishedSchema'
      { dryRun = Lude.Nothing,
        developmentSchemaARN = pDevelopmentSchemaARN_,
        publishedSchemaARN = pPublishedSchemaARN_,
        minorVersion = pMinorVersion_
      }

-- | Used for testing whether the Development schema provided is backwards compatible, or not, with the publish schema provided by the user to be upgraded. If schema compatibility fails, an exception would be thrown else the call would succeed. This parameter is optional and defaults to false.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsDryRun :: Lens.Lens' UpgradePublishedSchema (Lude.Maybe Lude.Bool)
upsDryRun = Lens.lens (dryRun :: UpgradePublishedSchema -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: UpgradePublishedSchema)
{-# DEPRECATED upsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ARN of the development schema with the changes used for the upgrade.
--
-- /Note:/ Consider using 'developmentSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsDevelopmentSchemaARN :: Lens.Lens' UpgradePublishedSchema Lude.Text
upsDevelopmentSchemaARN = Lens.lens (developmentSchemaARN :: UpgradePublishedSchema -> Lude.Text) (\s a -> s {developmentSchemaARN = a} :: UpgradePublishedSchema)
{-# DEPRECATED upsDevelopmentSchemaARN "Use generic-lens or generic-optics with 'developmentSchemaARN' instead." #-}

-- | The ARN of the published schema to be upgraded.
--
-- /Note:/ Consider using 'publishedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsPublishedSchemaARN :: Lens.Lens' UpgradePublishedSchema Lude.Text
upsPublishedSchemaARN = Lens.lens (publishedSchemaARN :: UpgradePublishedSchema -> Lude.Text) (\s a -> s {publishedSchemaARN = a} :: UpgradePublishedSchema)
{-# DEPRECATED upsPublishedSchemaARN "Use generic-lens or generic-optics with 'publishedSchemaARN' instead." #-}

-- | Identifies the minor version of the published schema that will be created. This parameter is NOT optional.
--
-- /Note:/ Consider using 'minorVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsMinorVersion :: Lens.Lens' UpgradePublishedSchema Lude.Text
upsMinorVersion = Lens.lens (minorVersion :: UpgradePublishedSchema -> Lude.Text) (\s a -> s {minorVersion = a} :: UpgradePublishedSchema)
{-# DEPRECATED upsMinorVersion "Use generic-lens or generic-optics with 'minorVersion' instead." #-}

instance Lude.AWSRequest UpgradePublishedSchema where
  type Rs UpgradePublishedSchema = UpgradePublishedSchemaResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpgradePublishedSchemaResponse'
            Lude.<$> (x Lude..?> "UpgradedSchemaArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpgradePublishedSchema where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpgradePublishedSchema where
  toJSON UpgradePublishedSchema' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DryRun" Lude..=) Lude.<$> dryRun,
            Lude.Just ("DevelopmentSchemaArn" Lude..= developmentSchemaARN),
            Lude.Just ("PublishedSchemaArn" Lude..= publishedSchemaARN),
            Lude.Just ("MinorVersion" Lude..= minorVersion)
          ]
      )

instance Lude.ToPath UpgradePublishedSchema where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/schema/upgradepublished"

instance Lude.ToQuery UpgradePublishedSchema where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpgradePublishedSchemaResponse' smart constructor.
data UpgradePublishedSchemaResponse = UpgradePublishedSchemaResponse'
  { upgradedSchemaARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpgradePublishedSchemaResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'upgradedSchemaARN' - The ARN of the upgraded schema that is returned as part of the response.
mkUpgradePublishedSchemaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpgradePublishedSchemaResponse
mkUpgradePublishedSchemaResponse pResponseStatus_ =
  UpgradePublishedSchemaResponse'
    { upgradedSchemaARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the upgraded schema that is returned as part of the response.
--
-- /Note:/ Consider using 'upgradedSchemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrsUpgradedSchemaARN :: Lens.Lens' UpgradePublishedSchemaResponse (Lude.Maybe Lude.Text)
upsrsUpgradedSchemaARN = Lens.lens (upgradedSchemaARN :: UpgradePublishedSchemaResponse -> Lude.Maybe Lude.Text) (\s a -> s {upgradedSchemaARN = a} :: UpgradePublishedSchemaResponse)
{-# DEPRECATED upsrsUpgradedSchemaARN "Use generic-lens or generic-optics with 'upgradedSchemaARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upsrsResponseStatus :: Lens.Lens' UpgradePublishedSchemaResponse Lude.Int
upsrsResponseStatus = Lens.lens (responseStatus :: UpgradePublishedSchemaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpgradePublishedSchemaResponse)
{-# DEPRECATED upsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
