{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.IntegrationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.IntegrationResponse
  ( IntegrationResponse (..),

    -- * Smart constructor
    mkIntegrationResponse,

    -- * Lenses
    intContentHandling,
    intResponseTemplates,
    intSelectionPattern,
    intStatusCode,
    intResponseParameters,
  )
where

import Network.AWS.APIGateway.Types.ContentHandlingStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an integration response. The status code must map to an existing 'MethodResponse' , and parameters and templates can be used to transform the back-end response.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Creating an API>
--
-- /See:/ 'mkIntegrationResponse' smart constructor.
data IntegrationResponse = IntegrationResponse'
  { contentHandling ::
      Lude.Maybe ContentHandlingStrategy,
    responseTemplates ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    selectionPattern :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe Lude.Text,
    responseParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntegrationResponse' with the minimum fields required to make a request.
--
-- * 'contentHandling' - Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
-- * 'responseParameters' - A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
-- * 'responseTemplates' - Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
-- * 'selectionPattern' - Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
-- * 'statusCode' - Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
mkIntegrationResponse ::
  IntegrationResponse
mkIntegrationResponse =
  IntegrationResponse'
    { contentHandling = Lude.Nothing,
      responseTemplates = Lude.Nothing,
      selectionPattern = Lude.Nothing,
      statusCode = Lude.Nothing,
      responseParameters = Lude.Nothing
    }

-- | Specifies how to handle response payload content type conversions. Supported values are @CONVERT_TO_BINARY@ and @CONVERT_TO_TEXT@ , with the following behaviors:
--
--
--     * @CONVERT_TO_BINARY@ : Converts a response payload from a Base64-encoded string to the corresponding binary blob.
--
--
--     * @CONVERT_TO_TEXT@ : Converts a response payload from a binary blob to a Base64-encoded string.
--
--
-- If this property is not defined, the response payload will be passed through from the integration response to the method response without modification.
--
-- /Note:/ Consider using 'contentHandling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
intContentHandling :: Lens.Lens' IntegrationResponse (Lude.Maybe ContentHandlingStrategy)
intContentHandling = Lens.lens (contentHandling :: IntegrationResponse -> Lude.Maybe ContentHandlingStrategy) (\s a -> s {contentHandling = a} :: IntegrationResponse)
{-# DEPRECATED intContentHandling "Use generic-lens or generic-optics with 'contentHandling' instead." #-}

-- | Specifies the templates used to transform the integration response body. Response templates are represented as a key/value map, with a content-type as the key and a template as the value.
--
-- /Note:/ Consider using 'responseTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
intResponseTemplates :: Lens.Lens' IntegrationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
intResponseTemplates = Lens.lens (responseTemplates :: IntegrationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseTemplates = a} :: IntegrationResponse)
{-# DEPRECATED intResponseTemplates "Use generic-lens or generic-optics with 'responseTemplates' instead." #-}

-- | Specifies the regular expression (regex) pattern used to choose an integration response based on the response from the back end. For example, if the success response returns nothing and the error response returns some string, you could use the @.+@ regex to match error response. However, make sure that the error response does not contain any newline (@\n@ ) character in such cases. If the back end is an AWS Lambda function, the AWS Lambda function error header is matched. For all other HTTP and AWS back ends, the HTTP status code is matched.
--
-- /Note:/ Consider using 'selectionPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
intSelectionPattern :: Lens.Lens' IntegrationResponse (Lude.Maybe Lude.Text)
intSelectionPattern = Lens.lens (selectionPattern :: IntegrationResponse -> Lude.Maybe Lude.Text) (\s a -> s {selectionPattern = a} :: IntegrationResponse)
{-# DEPRECATED intSelectionPattern "Use generic-lens or generic-optics with 'selectionPattern' instead." #-}

-- | Specifies the status code that is used to map the integration response to an existing 'MethodResponse' .
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
intStatusCode :: Lens.Lens' IntegrationResponse (Lude.Maybe Lude.Text)
intStatusCode = Lens.lens (statusCode :: IntegrationResponse -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: IntegrationResponse)
{-# DEPRECATED intStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | A key-value map specifying response parameters that are passed to the method response from the back end. The key is a method response header parameter name and the mapped value is an integration response header value, a static value enclosed within a pair of single quotes, or a JSON expression from the integration response body. The mapping key must match the pattern of @method.response.header.{name}@ , where @name@ is a valid and unique header name. The mapped non-static value must match the pattern of @integration.response.header.{name}@ or @integration.response.body.{JSON-expression}@ , where @name@ is a valid and unique response header name and @JSON-expression@ is a valid JSON expression without the @> @ prefix.
--
-- /Note:/ Consider using 'responseParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
intResponseParameters :: Lens.Lens' IntegrationResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
intResponseParameters = Lens.lens (responseParameters :: IntegrationResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {responseParameters = a} :: IntegrationResponse)
{-# DEPRECATED intResponseParameters "Use generic-lens or generic-optics with 'responseParameters' instead." #-}

instance Lude.FromJSON IntegrationResponse where
  parseJSON =
    Lude.withObject
      "IntegrationResponse"
      ( \x ->
          IntegrationResponse'
            Lude.<$> (x Lude..:? "contentHandling")
            Lude.<*> (x Lude..:? "responseTemplates" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "selectionPattern")
            Lude.<*> (x Lude..:? "statusCode")
            Lude.<*> (x Lude..:? "responseParameters" Lude..!= Lude.mempty)
      )
