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
-- Module      : Network.AWS.SSM.GetParametersByPath
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve parameters in a specific hierarchy. For more information, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-paramstore-working.html Working with Systems Manager Parameters> .
--
--
-- Request results are returned on a best-effort basis. If you specify @MaxResults@ in the request, the response includes information up to the limit specified. The number of items returned, however, can be between zero and the value of @MaxResults@ . If the service reaches an internal limit while processing the results, it stops the operation and returns the matching values up to that point and a @NextToken@ . You can specify the @NextToken@ in a subsequent call to get the next set of results.
--
module Network.AWS.SSM.GetParametersByPath
    (
    -- * Creating a Request
      getParametersByPath
    , GetParametersByPath
    -- * Request Lenses
    , gpbpWithDecryption
    , gpbpParameterFilters
    , gpbpNextToken
    , gpbpRecursive
    , gpbpMaxResults
    , gpbpPath

    -- * Destructuring the Response
    , getParametersByPathResponse
    , GetParametersByPathResponse
    -- * Response Lenses
    , gpbprsNextToken
    , gpbprsParameters
    , gpbprsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'getParametersByPath' smart constructor.
data GetParametersByPath = GetParametersByPath'
    { _gpbpWithDecryption   :: !(Maybe Bool)
    , _gpbpParameterFilters :: !(Maybe [ParameterStringFilter])
    , _gpbpNextToken        :: !(Maybe Text)
    , _gpbpRecursive        :: !(Maybe Bool)
    , _gpbpMaxResults       :: !(Maybe Nat)
    , _gpbpPath             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetParametersByPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpbpWithDecryption' - Retrieve all parameters in a hierarchy with their value decrypted.
--
-- * 'gpbpParameterFilters' - Filters to limit the request results.
--
-- * 'gpbpNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'gpbpRecursive' - Retrieve all parameters within a hierarchy.
--
-- * 'gpbpMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'gpbpPath' - The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A hierarchy can have a maximum of five levels. Examples: /Environment/Test/DBString003 /Finance/Prod/IAD/OS/WinServ2016/license15
getParametersByPath
    :: Text -- ^ 'gpbpPath'
    -> GetParametersByPath
getParametersByPath pPath_ =
    GetParametersByPath'
    { _gpbpWithDecryption = Nothing
    , _gpbpParameterFilters = Nothing
    , _gpbpNextToken = Nothing
    , _gpbpRecursive = Nothing
    , _gpbpMaxResults = Nothing
    , _gpbpPath = pPath_
    }

-- | Retrieve all parameters in a hierarchy with their value decrypted.
gpbpWithDecryption :: Lens' GetParametersByPath (Maybe Bool)
gpbpWithDecryption = lens _gpbpWithDecryption (\ s a -> s{_gpbpWithDecryption = a});

-- | Filters to limit the request results.
gpbpParameterFilters :: Lens' GetParametersByPath [ParameterStringFilter]
gpbpParameterFilters = lens _gpbpParameterFilters (\ s a -> s{_gpbpParameterFilters = a}) . _Default . _Coerce;

-- | A token to start the list. Use this token to get the next set of results.
gpbpNextToken :: Lens' GetParametersByPath (Maybe Text)
gpbpNextToken = lens _gpbpNextToken (\ s a -> s{_gpbpNextToken = a});

-- | Retrieve all parameters within a hierarchy.
gpbpRecursive :: Lens' GetParametersByPath (Maybe Bool)
gpbpRecursive = lens _gpbpRecursive (\ s a -> s{_gpbpRecursive = a});

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
gpbpMaxResults :: Lens' GetParametersByPath (Maybe Natural)
gpbpMaxResults = lens _gpbpMaxResults (\ s a -> s{_gpbpMaxResults = a}) . mapping _Nat;

-- | The hierarchy for the parameter. Hierarchies start with a forward slash (/) and end with the parameter name. A hierarchy can have a maximum of five levels. Examples: /Environment/Test/DBString003 /Finance/Prod/IAD/OS/WinServ2016/license15
gpbpPath :: Lens' GetParametersByPath Text
gpbpPath = lens _gpbpPath (\ s a -> s{_gpbpPath = a});

instance AWSRequest GetParametersByPath where
        type Rs GetParametersByPath =
             GetParametersByPathResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 GetParametersByPathResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Parameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetParametersByPath

instance NFData GetParametersByPath

instance ToHeaders GetParametersByPath where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.GetParametersByPath" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetParametersByPath where
        toJSON GetParametersByPath'{..}
          = object
              (catMaybes
                 [("WithDecryption" .=) <$> _gpbpWithDecryption,
                  ("ParameterFilters" .=) <$> _gpbpParameterFilters,
                  ("NextToken" .=) <$> _gpbpNextToken,
                  ("Recursive" .=) <$> _gpbpRecursive,
                  ("MaxResults" .=) <$> _gpbpMaxResults,
                  Just ("Path" .= _gpbpPath)])

instance ToPath GetParametersByPath where
        toPath = const "/"

instance ToQuery GetParametersByPath where
        toQuery = const mempty

-- | /See:/ 'getParametersByPathResponse' smart constructor.
data GetParametersByPathResponse = GetParametersByPathResponse'
    { _gpbprsNextToken      :: !(Maybe Text)
    , _gpbprsParameters     :: !(Maybe [Parameter])
    , _gpbprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetParametersByPathResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpbprsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'gpbprsParameters' - A list of parameters found in the specified hierarchy.
--
-- * 'gpbprsResponseStatus' - -- | The response status code.
getParametersByPathResponse
    :: Int -- ^ 'gpbprsResponseStatus'
    -> GetParametersByPathResponse
getParametersByPathResponse pResponseStatus_ =
    GetParametersByPathResponse'
    { _gpbprsNextToken = Nothing
    , _gpbprsParameters = Nothing
    , _gpbprsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. Use this token to get the next set of results.
gpbprsNextToken :: Lens' GetParametersByPathResponse (Maybe Text)
gpbprsNextToken = lens _gpbprsNextToken (\ s a -> s{_gpbprsNextToken = a});

-- | A list of parameters found in the specified hierarchy.
gpbprsParameters :: Lens' GetParametersByPathResponse [Parameter]
gpbprsParameters = lens _gpbprsParameters (\ s a -> s{_gpbprsParameters = a}) . _Default . _Coerce;

-- | -- | The response status code.
gpbprsResponseStatus :: Lens' GetParametersByPathResponse Int
gpbprsResponseStatus = lens _gpbprsResponseStatus (\ s a -> s{_gpbprsResponseStatus = a});

instance NFData GetParametersByPathResponse
