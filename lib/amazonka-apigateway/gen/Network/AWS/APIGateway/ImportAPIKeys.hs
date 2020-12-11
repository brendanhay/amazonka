{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.ImportAPIKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Import API keys from an external source, such as a CSV-formatted file.
module Network.AWS.APIGateway.ImportAPIKeys
  ( -- * Creating a request
    ImportAPIKeys (..),
    mkImportAPIKeys,

    -- ** Request lenses
    iakFailOnWarnings,
    iakBody,
    iakFormat,

    -- * Destructuring the response
    ImportAPIKeysResponse (..),
    mkImportAPIKeysResponse,

    -- ** Response lenses
    iakrsIds,
    iakrsWarnings,
    iakrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The POST request to import API keys from an external source, such as a CSV-formatted file.
--
-- /See:/ 'mkImportAPIKeys' smart constructor.
data ImportAPIKeys = ImportAPIKeys'
  { failOnWarnings ::
      Lude.Maybe Lude.Bool,
    body :: Lude.ByteString,
    format :: APIKeysFormat
  }
  deriving stock (Lude.Eq, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportAPIKeys' with the minimum fields required to make a request.
--
-- * 'body' - The payload of the POST request to import API keys. For the payload format, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format> .
-- * 'failOnWarnings' - A query parameter to indicate whether to rollback 'ApiKey' importation (@true@ ) or not (@false@ ) when error is encountered.
-- * 'format' - A query parameter to specify the input format to imported API keys. Currently, only the @csv@ format is supported.
mkImportAPIKeys ::
  -- | 'body'
  Lude.ByteString ->
  -- | 'format'
  APIKeysFormat ->
  ImportAPIKeys
mkImportAPIKeys pBody_ pFormat_ =
  ImportAPIKeys'
    { failOnWarnings = Lude.Nothing,
      body = pBody_,
      format = pFormat_
    }

-- | A query parameter to indicate whether to rollback 'ApiKey' importation (@true@ ) or not (@false@ ) when error is encountered.
--
-- /Note:/ Consider using 'failOnWarnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakFailOnWarnings :: Lens.Lens' ImportAPIKeys (Lude.Maybe Lude.Bool)
iakFailOnWarnings = Lens.lens (failOnWarnings :: ImportAPIKeys -> Lude.Maybe Lude.Bool) (\s a -> s {failOnWarnings = a} :: ImportAPIKeys)
{-# DEPRECATED iakFailOnWarnings "Use generic-lens or generic-optics with 'failOnWarnings' instead." #-}

-- | The payload of the POST request to import API keys. For the payload format, see <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-key-file-format.html API Key File Format> .
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakBody :: Lens.Lens' ImportAPIKeys Lude.ByteString
iakBody = Lens.lens (body :: ImportAPIKeys -> Lude.ByteString) (\s a -> s {body = a} :: ImportAPIKeys)
{-# DEPRECATED iakBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | A query parameter to specify the input format to imported API keys. Currently, only the @csv@ format is supported.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakFormat :: Lens.Lens' ImportAPIKeys APIKeysFormat
iakFormat = Lens.lens (format :: ImportAPIKeys -> APIKeysFormat) (\s a -> s {format = a} :: ImportAPIKeys)
{-# DEPRECATED iakFormat "Use generic-lens or generic-optics with 'format' instead." #-}

instance Lude.AWSRequest ImportAPIKeys where
  type Rs ImportAPIKeys = ImportAPIKeysResponse
  request = Req.postBody apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ImportAPIKeysResponse'
            Lude.<$> (x Lude..?> "ids" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "warnings" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToBody ImportAPIKeys where
  toBody = Lude.toBody Lude.. body

instance Lude.ToHeaders ImportAPIKeys where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath ImportAPIKeys where
  toPath = Lude.const "/apikeys"

instance Lude.ToQuery ImportAPIKeys where
  toQuery ImportAPIKeys' {..} =
    Lude.mconcat
      [ "failonwarnings" Lude.=: failOnWarnings,
        "format" Lude.=: format,
        "mode=import"
      ]

-- | The identifier of an 'ApiKey' used in a 'UsagePlan' .
--
-- /See:/ 'mkImportAPIKeysResponse' smart constructor.
data ImportAPIKeysResponse = ImportAPIKeysResponse'
  { ids ::
      Lude.Maybe [Lude.Text],
    warnings :: Lude.Maybe [Lude.Text],
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

-- | Creates a value of 'ImportAPIKeysResponse' with the minimum fields required to make a request.
--
-- * 'ids' - A list of all the 'ApiKey' identifiers.
-- * 'responseStatus' - The response status code.
-- * 'warnings' - A list of warning messages.
mkImportAPIKeysResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ImportAPIKeysResponse
mkImportAPIKeysResponse pResponseStatus_ =
  ImportAPIKeysResponse'
    { ids = Lude.Nothing,
      warnings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of all the 'ApiKey' identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakrsIds :: Lens.Lens' ImportAPIKeysResponse (Lude.Maybe [Lude.Text])
iakrsIds = Lens.lens (ids :: ImportAPIKeysResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {ids = a} :: ImportAPIKeysResponse)
{-# DEPRECATED iakrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | A list of warning messages.
--
-- /Note:/ Consider using 'warnings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakrsWarnings :: Lens.Lens' ImportAPIKeysResponse (Lude.Maybe [Lude.Text])
iakrsWarnings = Lens.lens (warnings :: ImportAPIKeysResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {warnings = a} :: ImportAPIKeysResponse)
{-# DEPRECATED iakrsWarnings "Use generic-lens or generic-optics with 'warnings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iakrsResponseStatus :: Lens.Lens' ImportAPIKeysResponse Lude.Int
iakrsResponseStatus = Lens.lens (responseStatus :: ImportAPIKeysResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ImportAPIKeysResponse)
{-# DEPRECATED iakrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
