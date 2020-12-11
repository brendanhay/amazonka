{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.PutRestAPI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A feature of the API Gateway control service for updating an existing API with an input of external API definitions. The update can take the form of merging the supplied definition into the existing API or overwriting the existing API.
module Network.AWS.APIGateway.PutRestAPI
  ( -- * Creating a request
    PutRestAPI (..),
    mkPutRestAPI,

    -- ** Request lenses
    praMode,
    praFailOnWarnings,
    praParameters,
    praRestAPIId,
    praBody,

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

-- | A PUT request to update an existing API, with external API definitions specified as the request body.
--
-- /See:/ 'mkPutRestAPI' smart constructor.
data PutRestAPI = PutRestAPI'
  { mode :: Lude.Maybe PutMode,
    failOnWarnings :: Lude.Maybe Lude.Bool,
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    restAPIId :: Lude.Text,
    body :: Lude.ByteString
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRestAPI' with the minimum fields required to make a request.
--
-- * 'body' - [Required] The PUT request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
-- * 'failOnWarnings' - A query parameter to indicate whether to rollback the API update (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
-- * 'mode' - The @mode@ query parameter to specify the update mode. Valid values are "merge" and "overwrite". By default, the update mode is "merge".
-- * 'parameters' - Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ .
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkPutRestAPI ::
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'body'
  Lude.ByteString ->
  PutRestAPI
mkPutRestAPI pRestAPIId_ pBody_ =
  PutRestAPI'
    { mode = Lude.Nothing,
      failOnWarnings = Lude.Nothing,
      parameters = Lude.Nothing,
      restAPIId = pRestAPIId_,
      body = pBody_
    }

-- | The @mode@ query parameter to specify the update mode. Valid values are "merge" and "overwrite". By default, the update mode is "merge".
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praMode :: Lens.Lens' PutRestAPI (Lude.Maybe PutMode)
praMode = Lens.lens (mode :: PutRestAPI -> Lude.Maybe PutMode) (\s a -> s {mode = a} :: PutRestAPI)
{-# DEPRECATED praMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | A query parameter to indicate whether to rollback the API update (@true@ ) or not (@false@ ) when a warning is encountered. The default value is @false@ .
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praFailOnWarnings :: Lens.Lens' PutRestAPI (Lude.Maybe Lude.Bool)
praFailOnWarnings = Lens.lens (failOnWarnings :: PutRestAPI -> Lude.Maybe Lude.Bool) (\s a -> s {failOnWarnings = a} :: PutRestAPI)
{-# DEPRECATED praFailOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead." #-}

-- | Custom header parameters as part of the request. For example, to exclude 'DocumentationParts' from an imported API, set @ignore=documentation@ as a @parameters@ value, as in the AWS CLI command of @aws apigateway import-rest-api --parameters ignore=documentation --body 'file:///path/to/imported-api-body.json'@ .
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praParameters :: Lens.Lens' PutRestAPI (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
praParameters = Lens.lens (parameters :: PutRestAPI -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: PutRestAPI)
{-# DEPRECATED praParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praRestAPIId :: Lens.Lens' PutRestAPI Lude.Text
praRestAPIId = Lens.lens (restAPIId :: PutRestAPI -> Lude.Text) (\s a -> s {restAPIId = a} :: PutRestAPI)
{-# DEPRECATED praRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | [Required] The PUT request body containing external API definitions. Currently, only OpenAPI definition JSON/YAML files are supported. The maximum size of the API definition file is 6MB.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praBody :: Lens.Lens' PutRestAPI Lude.ByteString
praBody = Lens.lens (body :: PutRestAPI -> Lude.ByteString) (\s a -> s {body = a} :: PutRestAPI)
{-# DEPRECATED praBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.AWSRequest PutRestAPI where
  type Rs PutRestAPI = RestAPI
  request = Req.putBody apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToBody PutRestAPI where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders PutRestAPI where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath PutRestAPI where
  toPath PutRestAPI' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId]

instance Lude.ToQuery PutRestAPI where
  toQuery PutRestAPI' {..} =
    Lude.mconcat
      [ "mode" Lude.=: mode,
        "failonwarnings" Lude.=: failOnWarnings,
        "parameters"
          Lude.=: Lude.toQuery
            (Lude.toQueryMap "entry" "key" "value" Lude.<$> parameters)
      ]
