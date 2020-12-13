{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing classifier (a @GrokClassifier@ , an @XMLClassifier@ , a @JsonClassifier@ , or a @CsvClassifier@ , depending on which field is present).
module Network.AWS.Glue.UpdateClassifier
  ( -- * Creating a request
    UpdateClassifier (..),
    mkUpdateClassifier,

    -- ** Request lenses
    ucGrokClassifier,
    ucXMLClassifier,
    ucCSVClassifier,
    ucJSONClassifier,

    -- * Destructuring the response
    UpdateClassifierResponse (..),
    mkUpdateClassifierResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateClassifier' smart constructor.
data UpdateClassifier = UpdateClassifier'
  { -- | A @GrokClassifier@ object with updated fields.
    grokClassifier :: Lude.Maybe UpdateGrokClassifierRequest,
    -- | An @XMLClassifier@ object with updated fields.
    xmlClassifier :: Lude.Maybe UpdateXMLClassifierRequest,
    -- | A @CsvClassifier@ object with updated fields.
    csvClassifier :: Lude.Maybe UpdateCSVClassifierRequest,
    -- | A @JsonClassifier@ object with updated fields.
    jsonClassifier :: Lude.Maybe UpdateJSONClassifierRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClassifier' with the minimum fields required to make a request.
--
-- * 'grokClassifier' - A @GrokClassifier@ object with updated fields.
-- * 'xmlClassifier' - An @XMLClassifier@ object with updated fields.
-- * 'csvClassifier' - A @CsvClassifier@ object with updated fields.
-- * 'jsonClassifier' - A @JsonClassifier@ object with updated fields.
mkUpdateClassifier ::
  UpdateClassifier
mkUpdateClassifier =
  UpdateClassifier'
    { grokClassifier = Lude.Nothing,
      xmlClassifier = Lude.Nothing,
      csvClassifier = Lude.Nothing,
      jsonClassifier = Lude.Nothing
    }

-- | A @GrokClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucGrokClassifier :: Lens.Lens' UpdateClassifier (Lude.Maybe UpdateGrokClassifierRequest)
ucGrokClassifier = Lens.lens (grokClassifier :: UpdateClassifier -> Lude.Maybe UpdateGrokClassifierRequest) (\s a -> s {grokClassifier = a} :: UpdateClassifier)
{-# DEPRECATED ucGrokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead." #-}

-- | An @XMLClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'xmlClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucXMLClassifier :: Lens.Lens' UpdateClassifier (Lude.Maybe UpdateXMLClassifierRequest)
ucXMLClassifier = Lens.lens (xmlClassifier :: UpdateClassifier -> Lude.Maybe UpdateXMLClassifierRequest) (\s a -> s {xmlClassifier = a} :: UpdateClassifier)
{-# DEPRECATED ucXMLClassifier "Use generic-lens or generic-optics with 'xmlClassifier' instead." #-}

-- | A @CsvClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucCSVClassifier :: Lens.Lens' UpdateClassifier (Lude.Maybe UpdateCSVClassifierRequest)
ucCSVClassifier = Lens.lens (csvClassifier :: UpdateClassifier -> Lude.Maybe UpdateCSVClassifierRequest) (\s a -> s {csvClassifier = a} :: UpdateClassifier)
{-# DEPRECATED ucCSVClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead." #-}

-- | A @JsonClassifier@ object with updated fields.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucJSONClassifier :: Lens.Lens' UpdateClassifier (Lude.Maybe UpdateJSONClassifierRequest)
ucJSONClassifier = Lens.lens (jsonClassifier :: UpdateClassifier -> Lude.Maybe UpdateJSONClassifierRequest) (\s a -> s {jsonClassifier = a} :: UpdateClassifier)
{-# DEPRECATED ucJSONClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead." #-}

instance Lude.AWSRequest UpdateClassifier where
  type Rs UpdateClassifier = UpdateClassifierResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateClassifierResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateClassifier" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateClassifier where
  toJSON UpdateClassifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GrokClassifier" Lude..=) Lude.<$> grokClassifier,
            ("XMLClassifier" Lude..=) Lude.<$> xmlClassifier,
            ("CsvClassifier" Lude..=) Lude.<$> csvClassifier,
            ("JsonClassifier" Lude..=) Lude.<$> jsonClassifier
          ]
      )

instance Lude.ToPath UpdateClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateClassifierResponse' smart constructor.
newtype UpdateClassifierResponse = UpdateClassifierResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateClassifierResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateClassifierResponse
mkUpdateClassifierResponse pResponseStatus_ =
  UpdateClassifierResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UpdateClassifierResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UpdateClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateClassifierResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
