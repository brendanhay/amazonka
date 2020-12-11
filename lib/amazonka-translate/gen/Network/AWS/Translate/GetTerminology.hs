{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.GetTerminology
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a custom terminology.
module Network.AWS.Translate.GetTerminology
  ( -- * Creating a request
    GetTerminology (..),
    mkGetTerminology,

    -- ** Request lenses
    gtName,
    gtTerminologyDataFormat,

    -- * Destructuring the response
    GetTerminologyResponse (..),
    mkGetTerminologyResponse,

    -- ** Response lenses
    gtrsTerminologyProperties,
    gtrsTerminologyDataLocation,
    gtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkGetTerminology' smart constructor.
data GetTerminology = GetTerminology'
  { name :: Lude.Text,
    terminologyDataFormat :: TerminologyDataFormat
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTerminology' with the minimum fields required to make a request.
--
-- * 'name' - The name of the custom terminology being retrieved.
-- * 'terminologyDataFormat' - The data format of the custom terminology being retrieved, either CSV or TMX.
mkGetTerminology ::
  -- | 'name'
  Lude.Text ->
  -- | 'terminologyDataFormat'
  TerminologyDataFormat ->
  GetTerminology
mkGetTerminology pName_ pTerminologyDataFormat_ =
  GetTerminology'
    { name = pName_,
      terminologyDataFormat = pTerminologyDataFormat_
    }

-- | The name of the custom terminology being retrieved.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTerminology Lude.Text
gtName = Lens.lens (name :: GetTerminology -> Lude.Text) (\s a -> s {name = a} :: GetTerminology)
{-# DEPRECATED gtName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The data format of the custom terminology being retrieved, either CSV or TMX.
--
-- /Note:/ Consider using 'terminologyDataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtTerminologyDataFormat :: Lens.Lens' GetTerminology TerminologyDataFormat
gtTerminologyDataFormat = Lens.lens (terminologyDataFormat :: GetTerminology -> TerminologyDataFormat) (\s a -> s {terminologyDataFormat = a} :: GetTerminology)
{-# DEPRECATED gtTerminologyDataFormat "Use generic-lens or generic-optics with 'terminologyDataFormat' instead." #-}

instance Lude.AWSRequest GetTerminology where
  type Rs GetTerminology = GetTerminologyResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTerminologyResponse'
            Lude.<$> (x Lude..?> "TerminologyProperties")
            Lude.<*> (x Lude..?> "TerminologyDataLocation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTerminology where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.GetTerminology" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTerminology where
  toJSON GetTerminology' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("TerminologyDataFormat" Lude..= terminologyDataFormat)
          ]
      )

instance Lude.ToPath GetTerminology where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTerminology where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTerminologyResponse' smart constructor.
data GetTerminologyResponse = GetTerminologyResponse'
  { terminologyProperties ::
      Lude.Maybe TerminologyProperties,
    terminologyDataLocation ::
      Lude.Maybe TerminologyDataLocation,
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

-- | Creates a value of 'GetTerminologyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'terminologyDataLocation' - The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
-- * 'terminologyProperties' - The properties of the custom terminology being retrieved.
mkGetTerminologyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTerminologyResponse
mkGetTerminologyResponse pResponseStatus_ =
  GetTerminologyResponse'
    { terminologyProperties = Lude.Nothing,
      terminologyDataLocation = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The properties of the custom terminology being retrieved.
--
-- /Note:/ Consider using 'terminologyProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTerminologyProperties :: Lens.Lens' GetTerminologyResponse (Lude.Maybe TerminologyProperties)
gtrsTerminologyProperties = Lens.lens (terminologyProperties :: GetTerminologyResponse -> Lude.Maybe TerminologyProperties) (\s a -> s {terminologyProperties = a} :: GetTerminologyResponse)
{-# DEPRECATED gtrsTerminologyProperties "Use generic-lens or generic-optics with 'terminologyProperties' instead." #-}

-- | The data location of the custom terminology being retrieved. The custom terminology file is returned in a presigned url that has a 30 minute expiration.
--
-- /Note:/ Consider using 'terminologyDataLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTerminologyDataLocation :: Lens.Lens' GetTerminologyResponse (Lude.Maybe TerminologyDataLocation)
gtrsTerminologyDataLocation = Lens.lens (terminologyDataLocation :: GetTerminologyResponse -> Lude.Maybe TerminologyDataLocation) (\s a -> s {terminologyDataLocation = a} :: GetTerminologyResponse)
{-# DEPRECATED gtrsTerminologyDataLocation "Use generic-lens or generic-optics with 'terminologyDataLocation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTerminologyResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTerminologyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTerminologyResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
