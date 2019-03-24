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
-- Module      : Network.AWS.SMS.CreateApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application. An application consists of one or more server groups. Each server group contain one or more servers.
--
--
module Network.AWS.SMS.CreateApp
    (
    -- * Creating a Request
      createApp
    , CreateApp
    -- * Request Lenses
    , caClientToken
    , caRoleName
    , caName
    , caDescription
    , caServerGroups
    , caTags

    -- * Destructuring the Response
    , createAppResponse
    , CreateAppResponse
    -- * Response Lenses
    , carsAppSummary
    , carsServerGroups
    , carsTags
    , carsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'createApp' smart constructor.
data CreateApp = CreateApp'
  { _caClientToken  :: !(Maybe Text)
  , _caRoleName     :: !(Maybe Text)
  , _caName         :: !(Maybe Text)
  , _caDescription  :: !(Maybe Text)
  , _caServerGroups :: !(Maybe [ServerGroup])
  , _caTags         :: !(Maybe [Tag])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caClientToken' - A unique, case-sensitive identifier you provide to ensure idempotency of application creation.
--
-- * 'caRoleName' - Name of service role in customer's account to be used by AWS SMS.
--
-- * 'caName' - Name of the new application.
--
-- * 'caDescription' - Description of the new application
--
-- * 'caServerGroups' - List of server groups to include in the application.
--
-- * 'caTags' - List of tags to be associated with the application.
createApp
    :: CreateApp
createApp =
  CreateApp'
    { _caClientToken = Nothing
    , _caRoleName = Nothing
    , _caName = Nothing
    , _caDescription = Nothing
    , _caServerGroups = Nothing
    , _caTags = Nothing
    }


-- | A unique, case-sensitive identifier you provide to ensure idempotency of application creation.
caClientToken :: Lens' CreateApp (Maybe Text)
caClientToken = lens _caClientToken (\ s a -> s{_caClientToken = a})

-- | Name of service role in customer's account to be used by AWS SMS.
caRoleName :: Lens' CreateApp (Maybe Text)
caRoleName = lens _caRoleName (\ s a -> s{_caRoleName = a})

-- | Name of the new application.
caName :: Lens' CreateApp (Maybe Text)
caName = lens _caName (\ s a -> s{_caName = a})

-- | Description of the new application
caDescription :: Lens' CreateApp (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | List of server groups to include in the application.
caServerGroups :: Lens' CreateApp [ServerGroup]
caServerGroups = lens _caServerGroups (\ s a -> s{_caServerGroups = a}) . _Default . _Coerce

-- | List of tags to be associated with the application.
caTags :: Lens' CreateApp [Tag]
caTags = lens _caTags (\ s a -> s{_caTags = a}) . _Default . _Coerce

instance AWSRequest CreateApp where
        type Rs CreateApp = CreateAppResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 CreateAppResponse' <$>
                   (x .?> "appSummary") <*>
                     (x .?> "serverGroups" .!@ mempty)
                     <*> (x .?> "tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable CreateApp where

instance NFData CreateApp where

instance ToHeaders CreateApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.CreateApp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateApp where
        toJSON CreateApp'{..}
          = object
              (catMaybes
                 [("clientToken" .=) <$> _caClientToken,
                  ("roleName" .=) <$> _caRoleName,
                  ("name" .=) <$> _caName,
                  ("description" .=) <$> _caDescription,
                  ("serverGroups" .=) <$> _caServerGroups,
                  ("tags" .=) <$> _caTags])

instance ToPath CreateApp where
        toPath = const "/"

instance ToQuery CreateApp where
        toQuery = const mempty

-- | /See:/ 'createAppResponse' smart constructor.
data CreateAppResponse = CreateAppResponse'
  { _carsAppSummary     :: !(Maybe AppSummary)
  , _carsServerGroups   :: !(Maybe [ServerGroup])
  , _carsTags           :: !(Maybe [Tag])
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsAppSummary' - Summary description of the application.
--
-- * 'carsServerGroups' - List of server groups included in the application.
--
-- * 'carsTags' - List of taags associated with the application.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAppResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAppResponse
createAppResponse pResponseStatus_ =
  CreateAppResponse'
    { _carsAppSummary = Nothing
    , _carsServerGroups = Nothing
    , _carsTags = Nothing
    , _carsResponseStatus = pResponseStatus_
    }


-- | Summary description of the application.
carsAppSummary :: Lens' CreateAppResponse (Maybe AppSummary)
carsAppSummary = lens _carsAppSummary (\ s a -> s{_carsAppSummary = a})

-- | List of server groups included in the application.
carsServerGroups :: Lens' CreateAppResponse [ServerGroup]
carsServerGroups = lens _carsServerGroups (\ s a -> s{_carsServerGroups = a}) . _Default . _Coerce

-- | List of taags associated with the application.
carsTags :: Lens' CreateAppResponse [Tag]
carsTags = lens _carsTags (\ s a -> s{_carsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAppResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAppResponse where
