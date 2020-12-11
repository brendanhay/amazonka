{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLoa
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the LOA-CFA for a connection, interconnect, or link aggregation group (LAG).
--
-- The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html Requesting Cross Connects at AWS Direct Connect Locations> in the /AWS Direct Connect User Guide/ .
module Network.AWS.DirectConnect.DescribeLoa
  ( -- * Creating a request
    DescribeLoa (..),
    mkDescribeLoa,

    -- ** Request lenses
    dlLoaContentType,
    dlProviderName,
    dlConnectionId,

    -- * Destructuring the response
    DescribeLoaResponse (..),
    mkDescribeLoaResponse,

    -- ** Response lenses
    dlrsLoaContent,
    dlrsLoaContentType,
    dlrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLoa' smart constructor.
data DescribeLoa = DescribeLoa'
  { loaContentType ::
      Lude.Maybe LoaContentType,
    providerName :: Lude.Maybe Lude.Text,
    connectionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoa' with the minimum fields required to make a request.
--
-- * 'connectionId' - The ID of a connection, LAG, or interconnect.
-- * 'loaContentType' - The standard media type for the LOA-CFA document. The only supported value is application/pdf.
-- * 'providerName' - The name of the service provider who establishes connectivity on your behalf. If you specify this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect.
mkDescribeLoa ::
  -- | 'connectionId'
  Lude.Text ->
  DescribeLoa
mkDescribeLoa pConnectionId_ =
  DescribeLoa'
    { loaContentType = Lude.Nothing,
      providerName = Lude.Nothing,
      connectionId = pConnectionId_
    }

-- | The standard media type for the LOA-CFA document. The only supported value is application/pdf.
--
-- /Note:/ Consider using 'loaContentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlLoaContentType :: Lens.Lens' DescribeLoa (Lude.Maybe LoaContentType)
dlLoaContentType = Lens.lens (loaContentType :: DescribeLoa -> Lude.Maybe LoaContentType) (\s a -> s {loaContentType = a} :: DescribeLoa)
{-# DEPRECATED dlLoaContentType "Use generic-lens or generic-optics with 'loaContentType' instead." #-}

-- | The name of the service provider who establishes connectivity on your behalf. If you specify this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect.
--
-- /Note:/ Consider using 'providerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlProviderName :: Lens.Lens' DescribeLoa (Lude.Maybe Lude.Text)
dlProviderName = Lens.lens (providerName :: DescribeLoa -> Lude.Maybe Lude.Text) (\s a -> s {providerName = a} :: DescribeLoa)
{-# DEPRECATED dlProviderName "Use generic-lens or generic-optics with 'providerName' instead." #-}

-- | The ID of a connection, LAG, or interconnect.
--
-- /Note:/ Consider using 'connectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlConnectionId :: Lens.Lens' DescribeLoa Lude.Text
dlConnectionId = Lens.lens (connectionId :: DescribeLoa -> Lude.Text) (\s a -> s {connectionId = a} :: DescribeLoa)
{-# DEPRECATED dlConnectionId "Use generic-lens or generic-optics with 'connectionId' instead." #-}

instance Lude.AWSRequest DescribeLoa where
  type Rs DescribeLoa = DescribeLoaResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLoaResponse'
            Lude.<$> (x Lude..?> "loaContent")
            Lude.<*> (x Lude..?> "loaContentType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLoa where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DescribeLoa" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLoa where
  toJSON DescribeLoa' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("loaContentType" Lude..=) Lude.<$> loaContentType,
            ("providerName" Lude..=) Lude.<$> providerName,
            Lude.Just ("connectionId" Lude..= connectionId)
          ]
      )

instance Lude.ToPath DescribeLoa where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoa where
  toQuery = Lude.const Lude.mempty

-- | Information about a Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.
--
-- /See:/ 'mkDescribeLoaResponse' smart constructor.
data DescribeLoaResponse = DescribeLoaResponse'
  { loaContent ::
      Lude.Maybe Lude.Base64,
    loaContentType :: Lude.Maybe LoaContentType,
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

-- | Creates a value of 'DescribeLoaResponse' with the minimum fields required to make a request.
--
-- * 'loaContent' - The binary contents of the LOA-CFA document.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'loaContentType' - The standard media type for the LOA-CFA document. The only supported value is application/pdf.
-- * 'responseStatus' - The response status code.
mkDescribeLoaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLoaResponse
mkDescribeLoaResponse pResponseStatus_ =
  DescribeLoaResponse'
    { loaContent = Lude.Nothing,
      loaContentType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The binary contents of the LOA-CFA document.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'loaContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsLoaContent :: Lens.Lens' DescribeLoaResponse (Lude.Maybe Lude.Base64)
dlrsLoaContent = Lens.lens (loaContent :: DescribeLoaResponse -> Lude.Maybe Lude.Base64) (\s a -> s {loaContent = a} :: DescribeLoaResponse)
{-# DEPRECATED dlrsLoaContent "Use generic-lens or generic-optics with 'loaContent' instead." #-}

-- | The standard media type for the LOA-CFA document. The only supported value is application/pdf.
--
-- /Note:/ Consider using 'loaContentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsLoaContentType :: Lens.Lens' DescribeLoaResponse (Lude.Maybe LoaContentType)
dlrsLoaContentType = Lens.lens (loaContentType :: DescribeLoaResponse -> Lude.Maybe LoaContentType) (\s a -> s {loaContentType = a} :: DescribeLoaResponse)
{-# DEPRECATED dlrsLoaContentType "Use generic-lens or generic-optics with 'loaContentType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlrsResponseStatus :: Lens.Lens' DescribeLoaResponse Lude.Int
dlrsResponseStatus = Lens.lens (responseStatus :: DescribeLoaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLoaResponse)
{-# DEPRECATED dlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
