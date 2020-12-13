{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetRestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the 'RestApi' resource in the collection.
module Network.AWS.APIGateway.GetRestAPI
  ( -- * Creating a request
    GetRestAPI (..),
    mkGetRestAPI,

    -- ** Request lenses
    graRestAPIId,

    -- * Destructuring the response
    RestAPI (..),
    mkRestAPI,

    -- ** Response lenses
    raMinimumCompressionSize,
    raDisableExecuteAPIEndpoint,
    raBinaryMediaTypes,
    raWarnings,
    raCreatedDate,
    raName,
    raVersion,
    raApiKeySource,
    raId,
    raPolicy,
    raEndpointConfiguration,
    raDescription,
    raTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The GET request to list an existing 'RestApi' defined for your collection.
--
-- /See:/ 'mkGetRestAPI' smart constructor.
newtype GetRestAPI = GetRestAPI'
  { -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRestAPI' with the minimum fields required to make a request.
--
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetRestAPI ::
  -- | 'restAPIId'
  Lude.Text ->
  GetRestAPI
mkGetRestAPI pRestAPIId_ = GetRestAPI' {restAPIId = pRestAPIId_}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
graRestAPIId :: Lens.Lens' GetRestAPI Lude.Text
graRestAPIId = Lens.lens (restAPIId :: GetRestAPI -> Lude.Text) (\s a -> s {restAPIId = a} :: GetRestAPI)
{-# DEPRECATED graRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest GetRestAPI where
  type Rs GetRestAPI = RestAPI
  request = Req.get apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetRestAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetRestAPI where
  toPath GetRestAPI' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId]

instance Lude.ToQuery GetRestAPI where
  toQuery = Lude.const Lude.mempty
