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
-- Module      : Network.AWS.StepFunctions.DescribeActivity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an activity.
--
--
module Network.AWS.StepFunctions.DescribeActivity
    (
    -- * Creating a Request
      describeActivity
    , DescribeActivity
    -- * Request Lenses
    , dActivityARN

    -- * Destructuring the Response
    , describeActivityResponse
    , DescribeActivityResponse
    -- * Response Lenses
    , desrsResponseStatus
    , desrsActivityARN
    , desrsName
    , desrsCreationDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'describeActivity' smart constructor.
newtype DescribeActivity = DescribeActivity'
  { _dActivityARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dActivityARN' - The Amazon Resource Name (ARN) of the activity to describe.
describeActivity
    :: Text -- ^ 'dActivityARN'
    -> DescribeActivity
describeActivity pActivityARN_ =
  DescribeActivity' {_dActivityARN = pActivityARN_}


-- | The Amazon Resource Name (ARN) of the activity to describe.
dActivityARN :: Lens' DescribeActivity Text
dActivityARN = lens _dActivityARN (\ s a -> s{_dActivityARN = a})

instance AWSRequest DescribeActivity where
        type Rs DescribeActivity = DescribeActivityResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 DescribeActivityResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "activityArn") <*>
                     (x .:> "name")
                     <*> (x .:> "creationDate"))

instance Hashable DescribeActivity where

instance NFData DescribeActivity where

instance ToHeaders DescribeActivity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.DescribeActivity" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeActivity where
        toJSON DescribeActivity'{..}
          = object
              (catMaybes [Just ("activityArn" .= _dActivityARN)])

instance ToPath DescribeActivity where
        toPath = const "/"

instance ToQuery DescribeActivity where
        toQuery = const mempty

-- | /See:/ 'describeActivityResponse' smart constructor.
data DescribeActivityResponse = DescribeActivityResponse'
  { _desrsResponseStatus :: !Int
  , _desrsActivityARN    :: !Text
  , _desrsName           :: !Text
  , _desrsCreationDate   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeActivityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsResponseStatus' - -- | The response status code.
--
-- * 'desrsActivityARN' - The Amazon Resource Name (ARN) that identifies the activity.
--
-- * 'desrsName' - The name of the activity. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
--
-- * 'desrsCreationDate' - The date the activity is created.
describeActivityResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> Text -- ^ 'desrsActivityARN'
    -> Text -- ^ 'desrsName'
    -> UTCTime -- ^ 'desrsCreationDate'
    -> DescribeActivityResponse
describeActivityResponse pResponseStatus_ pActivityARN_ pName_ pCreationDate_ =
  DescribeActivityResponse'
    { _desrsResponseStatus = pResponseStatus_
    , _desrsActivityARN = pActivityARN_
    , _desrsName = pName_
    , _desrsCreationDate = _Time # pCreationDate_
    }


-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeActivityResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the activity.
desrsActivityARN :: Lens' DescribeActivityResponse Text
desrsActivityARN = lens _desrsActivityARN (\ s a -> s{_desrsActivityARN = a})

-- | The name of the activity. A name must /not/ contain:     * whitespace     * brackets @< > { } [ ]@      * wildcard characters @? *@      * special characters @" # % \ ^ | ~ ` $ & , ; : /@      * control characters (@U+0000-001F@ , @U+007F-009F@ )
desrsName :: Lens' DescribeActivityResponse Text
desrsName = lens _desrsName (\ s a -> s{_desrsName = a})

-- | The date the activity is created.
desrsCreationDate :: Lens' DescribeActivityResponse UTCTime
desrsCreationDate = lens _desrsCreationDate (\ s a -> s{_desrsCreationDate = a}) . _Time

instance NFData DescribeActivityResponse where
