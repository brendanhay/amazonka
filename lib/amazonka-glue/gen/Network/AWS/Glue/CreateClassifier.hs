{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a classifier in the user's account. This can be a @GrokClassifier@ , an @XMLClassifier@ , a @JsonClassifier@ , or a @CsvClassifier@ , depending on which field of the request is present.
module Network.AWS.Glue.CreateClassifier
  ( -- * Creating a request
    CreateClassifier (..),
    mkCreateClassifier,

    -- ** Request lenses
    ccGrokClassifier,
    ccXMLClassifier,
    ccCSVClassifier,
    ccJSONClassifier,

    -- * Destructuring the response
    CreateClassifierResponse (..),
    mkCreateClassifierResponse,

    -- ** Response lenses
    ccrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateClassifier' smart constructor.
data CreateClassifier = CreateClassifier'
  { -- | A @GrokClassifier@ object specifying the classifier to create.
    grokClassifier :: Lude.Maybe CreateGrokClassifierRequest,
    -- | An @XMLClassifier@ object specifying the classifier to create.
    xmlClassifier :: Lude.Maybe CreateXMLClassifierRequest,
    -- | A @CsvClassifier@ object specifying the classifier to create.
    csvClassifier :: Lude.Maybe CreateCSVClassifierRequest,
    -- | A @JsonClassifier@ object specifying the classifier to create.
    jsonClassifier :: Lude.Maybe CreateJSONClassifierRequest
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClassifier' with the minimum fields required to make a request.
--
-- * 'grokClassifier' - A @GrokClassifier@ object specifying the classifier to create.
-- * 'xmlClassifier' - An @XMLClassifier@ object specifying the classifier to create.
-- * 'csvClassifier' - A @CsvClassifier@ object specifying the classifier to create.
-- * 'jsonClassifier' - A @JsonClassifier@ object specifying the classifier to create.
mkCreateClassifier ::
  CreateClassifier
mkCreateClassifier =
  CreateClassifier'
    { grokClassifier = Lude.Nothing,
      xmlClassifier = Lude.Nothing,
      csvClassifier = Lude.Nothing,
      jsonClassifier = Lude.Nothing
    }

-- | A @GrokClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'grokClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccGrokClassifier :: Lens.Lens' CreateClassifier (Lude.Maybe CreateGrokClassifierRequest)
ccGrokClassifier = Lens.lens (grokClassifier :: CreateClassifier -> Lude.Maybe CreateGrokClassifierRequest) (\s a -> s {grokClassifier = a} :: CreateClassifier)
{-# DEPRECATED ccGrokClassifier "Use generic-lens or generic-optics with 'grokClassifier' instead." #-}

-- | An @XMLClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'xmlClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccXMLClassifier :: Lens.Lens' CreateClassifier (Lude.Maybe CreateXMLClassifierRequest)
ccXMLClassifier = Lens.lens (xmlClassifier :: CreateClassifier -> Lude.Maybe CreateXMLClassifierRequest) (\s a -> s {xmlClassifier = a} :: CreateClassifier)
{-# DEPRECATED ccXMLClassifier "Use generic-lens or generic-optics with 'xmlClassifier' instead." #-}

-- | A @CsvClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'csvClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCSVClassifier :: Lens.Lens' CreateClassifier (Lude.Maybe CreateCSVClassifierRequest)
ccCSVClassifier = Lens.lens (csvClassifier :: CreateClassifier -> Lude.Maybe CreateCSVClassifierRequest) (\s a -> s {csvClassifier = a} :: CreateClassifier)
{-# DEPRECATED ccCSVClassifier "Use generic-lens or generic-optics with 'csvClassifier' instead." #-}

-- | A @JsonClassifier@ object specifying the classifier to create.
--
-- /Note:/ Consider using 'jsonClassifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccJSONClassifier :: Lens.Lens' CreateClassifier (Lude.Maybe CreateJSONClassifierRequest)
ccJSONClassifier = Lens.lens (jsonClassifier :: CreateClassifier -> Lude.Maybe CreateJSONClassifierRequest) (\s a -> s {jsonClassifier = a} :: CreateClassifier)
{-# DEPRECATED ccJSONClassifier "Use generic-lens or generic-optics with 'jsonClassifier' instead." #-}

instance Lude.AWSRequest CreateClassifier where
  type Rs CreateClassifier = CreateClassifierResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateClassifierResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.CreateClassifier" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateClassifier where
  toJSON CreateClassifier' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GrokClassifier" Lude..=) Lude.<$> grokClassifier,
            ("XMLClassifier" Lude..=) Lude.<$> xmlClassifier,
            ("CsvClassifier" Lude..=) Lude.<$> csvClassifier,
            ("JsonClassifier" Lude..=) Lude.<$> jsonClassifier
          ]
      )

instance Lude.ToPath CreateClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateClassifierResponse' smart constructor.
newtype CreateClassifierResponse = CreateClassifierResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateClassifierResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateClassifierResponse
mkCreateClassifierResponse pResponseStatus_ =
  CreateClassifierResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CreateClassifierResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CreateClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateClassifierResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
