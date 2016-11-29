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
-- Module      : Network.AWS.ServiceCatalog.DescribeConstraint
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information for a specified constraint.
--
--
module Network.AWS.ServiceCatalog.DescribeConstraint
    (
    -- * Creating a Request
      describeConstraint
    , DescribeConstraint
    -- * Request Lenses
    , dAcceptLanguage
    , dId

    -- * Destructuring the Response
    , describeConstraintResponse
    , DescribeConstraintResponse
    -- * Response Lenses
    , drsStatus
    , drsConstraintDetail
    , drsConstraintParameters
    , drsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.ServiceCatalog.Types
import           Network.AWS.ServiceCatalog.Types.Product

-- | /See:/ 'describeConstraint' smart constructor.
data DescribeConstraint = DescribeConstraint'
    { _dAcceptLanguage :: !(Maybe Text)
    , _dId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAcceptLanguage' - The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
--
-- * 'dId' - The identifier of the constraint.
describeConstraint
    :: Text -- ^ 'dId'
    -> DescribeConstraint
describeConstraint pId_ =
    DescribeConstraint'
    { _dAcceptLanguage = Nothing
    , _dId = pId_
    }

-- | The language code to use for this operation. Supported language codes are as follows: "en" (English) "jp" (Japanese) "zh" (Chinese) If no code is specified, "en" is used as the default.
dAcceptLanguage :: Lens' DescribeConstraint (Maybe Text)
dAcceptLanguage = lens _dAcceptLanguage (\ s a -> s{_dAcceptLanguage = a});

-- | The identifier of the constraint.
dId :: Lens' DescribeConstraint Text
dId = lens _dId (\ s a -> s{_dId = a});

instance AWSRequest DescribeConstraint where
        type Rs DescribeConstraint =
             DescribeConstraintResponse
        request = postJSON serviceCatalog
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConstraintResponse' <$>
                   (x .?> "Status") <*> (x .?> "ConstraintDetail") <*>
                     (x .?> "ConstraintParameters")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConstraint

instance NFData DescribeConstraint

instance ToHeaders DescribeConstraint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWS242ServiceCatalogService.DescribeConstraint" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeConstraint where
        toJSON DescribeConstraint'{..}
          = object
              (catMaybes
                 [("AcceptLanguage" .=) <$> _dAcceptLanguage,
                  Just ("Id" .= _dId)])

instance ToPath DescribeConstraint where
        toPath = const "/"

instance ToQuery DescribeConstraint where
        toQuery = const mempty

-- | /See:/ 'describeConstraintResponse' smart constructor.
data DescribeConstraintResponse = DescribeConstraintResponse'
    { _drsStatus               :: !(Maybe RequestStatus)
    , _drsConstraintDetail     :: !(Maybe ConstraintDetail)
    , _drsConstraintParameters :: !(Maybe Text)
    , _drsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConstraintResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsStatus' - The status of the current request.
--
-- * 'drsConstraintDetail' - Detailed constraint information.
--
-- * 'drsConstraintParameters' - The current parameters associated with the specified constraint.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeConstraintResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeConstraintResponse
describeConstraintResponse pResponseStatus_ =
    DescribeConstraintResponse'
    { _drsStatus = Nothing
    , _drsConstraintDetail = Nothing
    , _drsConstraintParameters = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The status of the current request.
drsStatus :: Lens' DescribeConstraintResponse (Maybe RequestStatus)
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});

-- | Detailed constraint information.
drsConstraintDetail :: Lens' DescribeConstraintResponse (Maybe ConstraintDetail)
drsConstraintDetail = lens _drsConstraintDetail (\ s a -> s{_drsConstraintDetail = a});

-- | The current parameters associated with the specified constraint.
drsConstraintParameters :: Lens' DescribeConstraintResponse (Maybe Text)
drsConstraintParameters = lens _drsConstraintParameters (\ s a -> s{_drsConstraintParameters = a});

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeConstraintResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

instance NFData DescribeConstraintResponse
