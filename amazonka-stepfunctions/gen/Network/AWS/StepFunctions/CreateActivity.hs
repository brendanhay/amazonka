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
-- Module      : Network.AWS.StepFunctions.CreateActivity
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an activity.
--
--
module Network.AWS.StepFunctions.CreateActivity
    (
    -- * Creating a Request
      createActivity
    , CreateActivity
    -- * Request Lenses
    , caName

    -- * Destructuring the Response
    , createActivityResponse
    , CreateActivityResponse
    -- * Response Lenses
    , carsResponseStatus
    , carsActivityARN
    , carsCreationDate
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StepFunctions.Types
import Network.AWS.StepFunctions.Types.Product

-- | /See:/ 'createActivity' smart constructor.
newtype CreateActivity = CreateActivity'
  { _caName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caName' - The name of the activity to create. This name must be unique for your AWS account and region.
createActivity
    :: Text -- ^ 'caName'
    -> CreateActivity
createActivity pName_ = CreateActivity' {_caName = pName_}


-- | The name of the activity to create. This name must be unique for your AWS account and region.
caName :: Lens' CreateActivity Text
caName = lens _caName (\ s a -> s{_caName = a});

instance AWSRequest CreateActivity where
        type Rs CreateActivity = CreateActivityResponse
        request = postJSON stepFunctions
        response
          = receiveJSON
              (\ s h x ->
                 CreateActivityResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "activityArn") <*>
                     (x .:> "creationDate"))

instance Hashable CreateActivity where

instance NFData CreateActivity where

instance ToHeaders CreateActivity where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSStepFunctions.CreateActivity" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON CreateActivity where
        toJSON CreateActivity'{..}
          = object (catMaybes [Just ("name" .= _caName)])

instance ToPath CreateActivity where
        toPath = const "/"

instance ToQuery CreateActivity where
        toQuery = const mempty

-- | /See:/ 'createActivityResponse' smart constructor.
data CreateActivityResponse = CreateActivityResponse'
  { _carsResponseStatus :: {-# NOUNPACK #-}!Int
  , _carsActivityARN    :: {-# NOUNPACK #-}!Text
  , _carsCreationDate   :: {-# NOUNPACK #-}!POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateActivityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsResponseStatus' - -- | The response status code.
--
-- * 'carsActivityARN' - The Amazon Resource Name (ARN) that identifies the created activity.
--
-- * 'carsCreationDate' - The date the activity was created.
createActivityResponse
    :: Int -- ^ 'carsResponseStatus'
    -> Text -- ^ 'carsActivityARN'
    -> UTCTime -- ^ 'carsCreationDate'
    -> CreateActivityResponse
createActivityResponse pResponseStatus_ pActivityARN_ pCreationDate_ =
  CreateActivityResponse'
  { _carsResponseStatus = pResponseStatus_
  , _carsActivityARN = pActivityARN_
  , _carsCreationDate = _Time # pCreationDate_
  }


-- | -- | The response status code.
carsResponseStatus :: Lens' CreateActivityResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a});

-- | The Amazon Resource Name (ARN) that identifies the created activity.
carsActivityARN :: Lens' CreateActivityResponse Text
carsActivityARN = lens _carsActivityARN (\ s a -> s{_carsActivityARN = a});

-- | The date the activity was created.
carsCreationDate :: Lens' CreateActivityResponse UTCTime
carsCreationDate = lens _carsCreationDate (\ s a -> s{_carsCreationDate = a}) . _Time;

instance NFData CreateActivityResponse where
