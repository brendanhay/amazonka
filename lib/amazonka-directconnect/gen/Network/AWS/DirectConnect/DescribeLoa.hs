{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeLoa
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the LOA-CFA for a connection, interconnect, or link aggregation group (LAG).
--
--
-- The Letter of Authorization - Connecting Facility Assignment (LOA-CFA) is a document that is used when establishing your cross connect to AWS at the colocation facility. For more information, see <http://docs.aws.amazon.com/directconnect/latest/UserGuide/Colocation.html Requesting Cross Connects at AWS Direct Connect Locations> in the AWS Direct Connect user guide.
--
module Network.AWS.DirectConnect.DescribeLoa
    (
    -- * Creating a Request
      describeLoa
    , DescribeLoa
    -- * Request Lenses
    , dlLoaContentType
    , dlProviderName
    , dlConnectionId

    -- * Destructuring the Response
    , describeLoaResponse
    , DescribeLoaResponse
    -- * Response Lenses
    , dlrsLoaContent
    , dlrsLoaContentType
    , dlrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeLoa operation.
--
--
--
-- /See:/ 'describeLoa' smart constructor.
data DescribeLoa = DescribeLoa'
  { _dlLoaContentType :: !(Maybe LoaContentType)
  , _dlProviderName   :: !(Maybe Text)
  , _dlConnectionId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoa' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlLoaContentType' - A standard media type indicating the content type of the LOA-CFA document. Currently, the only supported value is "application/pdf". Default: application/pdf
--
-- * 'dlProviderName' - The name of the service provider who establishes connectivity on your behalf. If you supply this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect. Default: None
--
-- * 'dlConnectionId' - The ID of a connection, LAG, or interconnect for which to get the LOA-CFA information. Example: dxcon-abc123 or dxlag-abc123 Default: None
describeLoa
    :: Text -- ^ 'dlConnectionId'
    -> DescribeLoa
describeLoa pConnectionId_ =
  DescribeLoa'
    { _dlLoaContentType = Nothing
    , _dlProviderName = Nothing
    , _dlConnectionId = pConnectionId_
    }


-- | A standard media type indicating the content type of the LOA-CFA document. Currently, the only supported value is "application/pdf". Default: application/pdf
dlLoaContentType :: Lens' DescribeLoa (Maybe LoaContentType)
dlLoaContentType = lens _dlLoaContentType (\ s a -> s{_dlLoaContentType = a})

-- | The name of the service provider who establishes connectivity on your behalf. If you supply this parameter, the LOA-CFA lists the provider name alongside your company name as the requester of the cross connect. Default: None
dlProviderName :: Lens' DescribeLoa (Maybe Text)
dlProviderName = lens _dlProviderName (\ s a -> s{_dlProviderName = a})

-- | The ID of a connection, LAG, or interconnect for which to get the LOA-CFA information. Example: dxcon-abc123 or dxlag-abc123 Default: None
dlConnectionId :: Lens' DescribeLoa Text
dlConnectionId = lens _dlConnectionId (\ s a -> s{_dlConnectionId = a})

instance AWSRequest DescribeLoa where
        type Rs DescribeLoa = DescribeLoaResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLoaResponse' <$>
                   (x .?> "loaContent") <*> (x .?> "loaContentType") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeLoa where

instance NFData DescribeLoa where

instance ToHeaders DescribeLoa where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeLoa" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLoa where
        toJSON DescribeLoa'{..}
          = object
              (catMaybes
                 [("loaContentType" .=) <$> _dlLoaContentType,
                  ("providerName" .=) <$> _dlProviderName,
                  Just ("connectionId" .= _dlConnectionId)])

instance ToPath DescribeLoa where
        toPath = const "/"

instance ToQuery DescribeLoa where
        toQuery = const mempty

-- | A structure containing the Letter of Authorization - Connecting Facility Assignment (LOA-CFA) for a connection.
--
--
--
-- /See:/ 'describeLoaResponse' smart constructor.
data DescribeLoaResponse = DescribeLoaResponse'
  { _dlrsLoaContent     :: !(Maybe Base64)
  , _dlrsLoaContentType :: !(Maybe LoaContentType)
  , _dlrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLoaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsLoaContent' - Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'dlrsLoaContentType' - Undocumented member.
--
-- * 'dlrsResponseStatus' - -- | The response status code.
describeLoaResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> DescribeLoaResponse
describeLoaResponse pResponseStatus_ =
  DescribeLoaResponse'
    { _dlrsLoaContent = Nothing
    , _dlrsLoaContentType = Nothing
    , _dlrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
dlrsLoaContent :: Lens' DescribeLoaResponse (Maybe ByteString)
dlrsLoaContent = lens _dlrsLoaContent (\ s a -> s{_dlrsLoaContent = a}) . mapping _Base64

-- | Undocumented member.
dlrsLoaContentType :: Lens' DescribeLoaResponse (Maybe LoaContentType)
dlrsLoaContentType = lens _dlrsLoaContentType (\ s a -> s{_dlrsLoaContentType = a})

-- | -- | The response status code.
dlrsResponseStatus :: Lens' DescribeLoaResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

instance NFData DescribeLoaResponse where
