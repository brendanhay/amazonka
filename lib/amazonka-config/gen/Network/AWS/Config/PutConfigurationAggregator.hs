{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutConfigurationAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the configuration aggregator with the selected source accounts and regions. The source account can be individual account(s) or an organization.
module Network.AWS.Config.PutConfigurationAggregator
  ( -- * Creating a request
    PutConfigurationAggregator (..),
    mkPutConfigurationAggregator,

    -- ** Request lenses
    pcaOrganizationAggregationSource,
    pcaAccountAggregationSources,
    pcaTags,
    pcaConfigurationAggregatorName,

    -- * Destructuring the response
    PutConfigurationAggregatorResponse (..),
    mkPutConfigurationAggregatorResponse,

    -- ** Response lenses
    pcarsConfigurationAggregator,
    pcarsResponseStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutConfigurationAggregator' smart constructor.
data PutConfigurationAggregator = PutConfigurationAggregator'
  { organizationAggregationSource ::
      Lude.Maybe
        OrganizationAggregationSource,
    accountAggregationSources ::
      Lude.Maybe [AccountAggregationSource],
    tags :: Lude.Maybe [Tag],
    configurationAggregatorName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConfigurationAggregator' with the minimum fields required to make a request.
--
-- * 'accountAggregationSources' - A list of AccountAggregationSource object.
-- * 'configurationAggregatorName' - The name of the configuration aggregator.
-- * 'organizationAggregationSource' - An OrganizationAggregationSource object.
-- * 'tags' - An array of tag object.
mkPutConfigurationAggregator ::
  -- | 'configurationAggregatorName'
  Lude.Text ->
  PutConfigurationAggregator
mkPutConfigurationAggregator pConfigurationAggregatorName_ =
  PutConfigurationAggregator'
    { organizationAggregationSource =
        Lude.Nothing,
      accountAggregationSources = Lude.Nothing,
      tags = Lude.Nothing,
      configurationAggregatorName = pConfigurationAggregatorName_
    }

-- | An OrganizationAggregationSource object.
--
-- /Note:/ Consider using 'organizationAggregationSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaOrganizationAggregationSource :: Lens.Lens' PutConfigurationAggregator (Lude.Maybe OrganizationAggregationSource)
pcaOrganizationAggregationSource = Lens.lens (organizationAggregationSource :: PutConfigurationAggregator -> Lude.Maybe OrganizationAggregationSource) (\s a -> s {organizationAggregationSource = a} :: PutConfigurationAggregator)
{-# DEPRECATED pcaOrganizationAggregationSource "Use generic-lens or generic-optics with 'organizationAggregationSource' instead." #-}

-- | A list of AccountAggregationSource object.
--
-- /Note:/ Consider using 'accountAggregationSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaAccountAggregationSources :: Lens.Lens' PutConfigurationAggregator (Lude.Maybe [AccountAggregationSource])
pcaAccountAggregationSources = Lens.lens (accountAggregationSources :: PutConfigurationAggregator -> Lude.Maybe [AccountAggregationSource]) (\s a -> s {accountAggregationSources = a} :: PutConfigurationAggregator)
{-# DEPRECATED pcaAccountAggregationSources "Use generic-lens or generic-optics with 'accountAggregationSources' instead." #-}

-- | An array of tag object.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaTags :: Lens.Lens' PutConfigurationAggregator (Lude.Maybe [Tag])
pcaTags = Lens.lens (tags :: PutConfigurationAggregator -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PutConfigurationAggregator)
{-# DEPRECATED pcaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcaConfigurationAggregatorName :: Lens.Lens' PutConfigurationAggregator Lude.Text
pcaConfigurationAggregatorName = Lens.lens (configurationAggregatorName :: PutConfigurationAggregator -> Lude.Text) (\s a -> s {configurationAggregatorName = a} :: PutConfigurationAggregator)
{-# DEPRECATED pcaConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

instance Lude.AWSRequest PutConfigurationAggregator where
  type
    Rs PutConfigurationAggregator =
      PutConfigurationAggregatorResponse
  request = Req.postJSON configService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutConfigurationAggregatorResponse'
            Lude.<$> (x Lude..?> "ConfigurationAggregator")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutConfigurationAggregator where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.PutConfigurationAggregator" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutConfigurationAggregator where
  toJSON PutConfigurationAggregator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OrganizationAggregationSource" Lude..=)
              Lude.<$> organizationAggregationSource,
            ("AccountAggregationSources" Lude..=)
              Lude.<$> accountAggregationSources,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just
              ( "ConfigurationAggregatorName"
                  Lude..= configurationAggregatorName
              )
          ]
      )

instance Lude.ToPath PutConfigurationAggregator where
  toPath = Lude.const "/"

instance Lude.ToQuery PutConfigurationAggregator where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutConfigurationAggregatorResponse' smart constructor.
data PutConfigurationAggregatorResponse = PutConfigurationAggregatorResponse'
  { configurationAggregator ::
      Lude.Maybe
        ConfigurationAggregator,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutConfigurationAggregatorResponse' with the minimum fields required to make a request.
--
-- * 'configurationAggregator' - Returns a ConfigurationAggregator object.
-- * 'responseStatus' - The response status code.
mkPutConfigurationAggregatorResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutConfigurationAggregatorResponse
mkPutConfigurationAggregatorResponse pResponseStatus_ =
  PutConfigurationAggregatorResponse'
    { configurationAggregator =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a ConfigurationAggregator object.
--
-- /Note:/ Consider using 'configurationAggregator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcarsConfigurationAggregator :: Lens.Lens' PutConfigurationAggregatorResponse (Lude.Maybe ConfigurationAggregator)
pcarsConfigurationAggregator = Lens.lens (configurationAggregator :: PutConfigurationAggregatorResponse -> Lude.Maybe ConfigurationAggregator) (\s a -> s {configurationAggregator = a} :: PutConfigurationAggregatorResponse)
{-# DEPRECATED pcarsConfigurationAggregator "Use generic-lens or generic-optics with 'configurationAggregator' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcarsResponseStatus :: Lens.Lens' PutConfigurationAggregatorResponse Lude.Int
pcarsResponseStatus = Lens.lens (responseStatus :: PutConfigurationAggregatorResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutConfigurationAggregatorResponse)
{-# DEPRECATED pcarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
