{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the current 'ApiKey' resource.
module Network.AWS.APIGateway.GetAPIKey
  ( -- * Creating a request
    GetAPIKey (..),
    mkGetAPIKey,

    -- ** Request lenses
    gakIncludeValue,
    gakApiKey,

    -- * Destructuring the response
    APIKey (..),
    mkAPIKey,

    -- ** Response lenses
    akEnabled,
    akValue,
    akCustomerId,
    akCreatedDate,
    akName,
    akId,
    akStageKeys,
    akLastUpdatedDate,
    akDescription,
    akTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | A request to get information about the current 'ApiKey' resource.
--
-- /See:/ 'mkGetAPIKey' smart constructor.
data GetAPIKey = GetAPIKey'
  { -- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
    includeValue :: Lude.Maybe Lude.Bool,
    -- | [Required] The identifier of the 'ApiKey' resource.
    apiKey :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetAPIKey' with the minimum fields required to make a request.
--
-- * 'includeValue' - A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
-- * 'apiKey' - [Required] The identifier of the 'ApiKey' resource.
mkGetAPIKey ::
  -- | 'apiKey'
  Lude.Text ->
  GetAPIKey
mkGetAPIKey pApiKey_ =
  GetAPIKey' {includeValue = Lude.Nothing, apiKey = pApiKey_}

-- | A boolean flag to specify whether (@true@ ) or not (@false@ ) the result contains the key value.
--
-- /Note:/ Consider using 'includeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakIncludeValue :: Lens.Lens' GetAPIKey (Lude.Maybe Lude.Bool)
gakIncludeValue = Lens.lens (includeValue :: GetAPIKey -> Lude.Maybe Lude.Bool) (\s a -> s {includeValue = a} :: GetAPIKey)
{-# DEPRECATED gakIncludeValue "Use generic-lens or generic-optics with 'includeValue' instead." #-}

-- | [Required] The identifier of the 'ApiKey' resource.
--
-- /Note:/ Consider using 'apiKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gakApiKey :: Lens.Lens' GetAPIKey Lude.Text
gakApiKey = Lens.lens (apiKey :: GetAPIKey -> Lude.Text) (\s a -> s {apiKey = a} :: GetAPIKey)
{-# DEPRECATED gakApiKey "Use generic-lens or generic-optics with 'apiKey' instead." #-}

instance Lude.AWSRequest GetAPIKey where
  type Rs GetAPIKey = APIKey
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetAPIKey where
  toPath GetAPIKey' {..} =
    Lude.mconcat ["/apikeys/", Lude.toBS apiKey]

instance Lude.ToQuery GetAPIKey where
  toQuery GetAPIKey' {..} =
    Lude.mconcat ["includeValue" Lude.=: includeValue]
