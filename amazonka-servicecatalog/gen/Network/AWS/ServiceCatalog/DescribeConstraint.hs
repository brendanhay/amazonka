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
    , desrsStatus
    , desrsConstraintDetail
    , desrsConstraintParameters
    , desrsResponseStatus
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
    { _desrsStatus               :: !(Maybe RequestStatus)
    , _desrsConstraintDetail     :: !(Maybe ConstraintDetail)
    , _desrsConstraintParameters :: !(Maybe Text)
    , _desrsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeConstraintResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsStatus' - The status of the current request.
--
-- * 'desrsConstraintDetail' - Detailed constraint information.
--
-- * 'desrsConstraintParameters' - The current parameters associated with the specified constraint.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeConstraintResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeConstraintResponse
describeConstraintResponse pResponseStatus_ =
    DescribeConstraintResponse'
    { _desrsStatus = Nothing
    , _desrsConstraintDetail = Nothing
    , _desrsConstraintParameters = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }

-- | The status of the current request.
desrsStatus :: Lens' DescribeConstraintResponse (Maybe RequestStatus)
desrsStatus = lens _desrsStatus (\ s a -> s{_desrsStatus = a});

-- | Detailed constraint information.
desrsConstraintDetail :: Lens' DescribeConstraintResponse (Maybe ConstraintDetail)
desrsConstraintDetail = lens _desrsConstraintDetail (\ s a -> s{_desrsConstraintDetail = a});

-- | The current parameters associated with the specified constraint.
desrsConstraintParameters :: Lens' DescribeConstraintResponse (Maybe Text)
desrsConstraintParameters = lens _desrsConstraintParameters (\ s a -> s{_desrsConstraintParameters = a});

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeConstraintResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a});

instance NFData DescribeConstraintResponse
