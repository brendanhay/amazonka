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
-- Module      : Network.AWS.DirectoryService.DescribeConditionalForwarders
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the conditional forwarders for this account.
--
--
-- If no input parameters are provided for RemoteDomainNames, this request describes all conditional forwarders for the specified directory ID.
--
module Network.AWS.DirectoryService.DescribeConditionalForwarders
    (
    -- * Creating a Request
      describeConditionalForwarders
    , DescribeConditionalForwarders
    -- * Request Lenses
    , dcfRemoteDomainNames
    , dcfDirectoryId

    -- * Destructuring the Response
    , describeConditionalForwardersResponse
    , DescribeConditionalForwardersResponse
    -- * Response Lenses
    , dcfrsConditionalForwarders
    , dcfrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Describes a conditional forwarder.
--
--
--
-- /See:/ 'describeConditionalForwarders' smart constructor.
data DescribeConditionalForwarders = DescribeConditionalForwarders'
  { _dcfRemoteDomainNames :: !(Maybe [Text])
  , _dcfDirectoryId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConditionalForwarders' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfRemoteDomainNames' - The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
--
-- * 'dcfDirectoryId' - The directory ID for which to get the list of associated conditional forwarders.
describeConditionalForwarders
    :: Text -- ^ 'dcfDirectoryId'
    -> DescribeConditionalForwarders
describeConditionalForwarders pDirectoryId_ =
  DescribeConditionalForwarders'
    {_dcfRemoteDomainNames = Nothing, _dcfDirectoryId = pDirectoryId_}


-- | The fully qualified domain names (FQDN) of the remote domains for which to get the list of associated conditional forwarders. If this member is null, all conditional forwarders are returned.
dcfRemoteDomainNames :: Lens' DescribeConditionalForwarders [Text]
dcfRemoteDomainNames = lens _dcfRemoteDomainNames (\ s a -> s{_dcfRemoteDomainNames = a}) . _Default . _Coerce

-- | The directory ID for which to get the list of associated conditional forwarders.
dcfDirectoryId :: Lens' DescribeConditionalForwarders Text
dcfDirectoryId = lens _dcfDirectoryId (\ s a -> s{_dcfDirectoryId = a})

instance AWSRequest DescribeConditionalForwarders
         where
        type Rs DescribeConditionalForwarders =
             DescribeConditionalForwardersResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConditionalForwardersResponse' <$>
                   (x .?> "ConditionalForwarders" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeConditionalForwarders where

instance NFData DescribeConditionalForwarders where

instance ToHeaders DescribeConditionalForwarders
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DescribeConditionalForwarders"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConditionalForwarders where
        toJSON DescribeConditionalForwarders'{..}
          = object
              (catMaybes
                 [("RemoteDomainNames" .=) <$> _dcfRemoteDomainNames,
                  Just ("DirectoryId" .= _dcfDirectoryId)])

instance ToPath DescribeConditionalForwarders where
        toPath = const "/"

instance ToQuery DescribeConditionalForwarders where
        toQuery = const mempty

-- | The result of a DescribeConditionalForwarder request.
--
--
--
-- /See:/ 'describeConditionalForwardersResponse' smart constructor.
data DescribeConditionalForwardersResponse = DescribeConditionalForwardersResponse'
  { _dcfrsConditionalForwarders :: !(Maybe [ConditionalForwarder])
  , _dcfrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConditionalForwardersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcfrsConditionalForwarders' - The list of conditional forwarders that have been created.
--
-- * 'dcfrsResponseStatus' - -- | The response status code.
describeConditionalForwardersResponse
    :: Int -- ^ 'dcfrsResponseStatus'
    -> DescribeConditionalForwardersResponse
describeConditionalForwardersResponse pResponseStatus_ =
  DescribeConditionalForwardersResponse'
    { _dcfrsConditionalForwarders = Nothing
    , _dcfrsResponseStatus = pResponseStatus_
    }


-- | The list of conditional forwarders that have been created.
dcfrsConditionalForwarders :: Lens' DescribeConditionalForwardersResponse [ConditionalForwarder]
dcfrsConditionalForwarders = lens _dcfrsConditionalForwarders (\ s a -> s{_dcfrsConditionalForwarders = a}) . _Default . _Coerce

-- | -- | The response status code.
dcfrsResponseStatus :: Lens' DescribeConditionalForwardersResponse Int
dcfrsResponseStatus = lens _dcfrsResponseStatus (\ s a -> s{_dcfrsResponseStatus = a})

instance NFData DescribeConditionalForwardersResponse
         where
