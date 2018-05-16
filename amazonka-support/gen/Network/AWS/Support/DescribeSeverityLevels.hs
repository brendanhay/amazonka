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
-- Module      : Network.AWS.Support.DescribeSeverityLevels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of severity levels that you can assign to an AWS Support case. The severity level for a case is also a field in the 'CaseDetails' data type included in any 'CreateCase' request.
--
--
module Network.AWS.Support.DescribeSeverityLevels
    (
    -- * Creating a Request
      describeSeverityLevels
    , DescribeSeverityLevels
    -- * Request Lenses
    , dslLanguage

    -- * Destructuring the Response
    , describeSeverityLevelsResponse
    , DescribeSeverityLevelsResponse
    -- * Response Lenses
    , dslrsSeverityLevels
    , dslrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Support.Types
import Network.AWS.Support.Types.Product

-- |
--
--
--
-- /See:/ 'describeSeverityLevels' smart constructor.
newtype DescribeSeverityLevels = DescribeSeverityLevels'
  { _dslLanguage :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSeverityLevels' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dslLanguage' - The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
describeSeverityLevels
    :: DescribeSeverityLevels
describeSeverityLevels = DescribeSeverityLevels' {_dslLanguage = Nothing}


-- | The ISO 639-1 code for the language in which AWS provides support. AWS Support currently supports English ("en") and Japanese ("ja"). Language parameters must be passed explicitly for operations that take them.
dslLanguage :: Lens' DescribeSeverityLevels (Maybe Text)
dslLanguage = lens _dslLanguage (\ s a -> s{_dslLanguage = a})

instance AWSRequest DescribeSeverityLevels where
        type Rs DescribeSeverityLevels =
             DescribeSeverityLevelsResponse
        request = postJSON support
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSeverityLevelsResponse' <$>
                   (x .?> "severityLevels" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeSeverityLevels where

instance NFData DescribeSeverityLevels where

instance ToHeaders DescribeSeverityLevels where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSSupport_20130415.DescribeSeverityLevels" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSeverityLevels where
        toJSON DescribeSeverityLevels'{..}
          = object
              (catMaybes [("language" .=) <$> _dslLanguage])

instance ToPath DescribeSeverityLevels where
        toPath = const "/"

instance ToQuery DescribeSeverityLevels where
        toQuery = const mempty

-- | The list of severity levels returned by the 'DescribeSeverityLevels' operation.
--
--
--
-- /See:/ 'describeSeverityLevelsResponse' smart constructor.
data DescribeSeverityLevelsResponse = DescribeSeverityLevelsResponse'
  { _dslrsSeverityLevels :: !(Maybe [SeverityLevel])
  , _dslrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSeverityLevelsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dslrsSeverityLevels' - The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
--
-- * 'dslrsResponseStatus' - -- | The response status code.
describeSeverityLevelsResponse
    :: Int -- ^ 'dslrsResponseStatus'
    -> DescribeSeverityLevelsResponse
describeSeverityLevelsResponse pResponseStatus_ =
  DescribeSeverityLevelsResponse'
    {_dslrsSeverityLevels = Nothing, _dslrsResponseStatus = pResponseStatus_}


-- | The available severity levels for the support case. Available severity levels are defined by your service level agreement with AWS.
dslrsSeverityLevels :: Lens' DescribeSeverityLevelsResponse [SeverityLevel]
dslrsSeverityLevels = lens _dslrsSeverityLevels (\ s a -> s{_dslrsSeverityLevels = a}) . _Default . _Coerce

-- | -- | The response status code.
dslrsResponseStatus :: Lens' DescribeSeverityLevelsResponse Int
dslrsResponseStatus = lens _dslrsResponseStatus (\ s a -> s{_dslrsResponseStatus = a})

instance NFData DescribeSeverityLevelsResponse where
