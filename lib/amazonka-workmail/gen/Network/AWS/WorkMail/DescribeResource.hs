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
-- Module      : Network.AWS.WorkMail.DescribeResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the resource.
--
--
module Network.AWS.WorkMail.DescribeResource
    (
    -- * Creating a Request
      describeResource
    , DescribeResource
    -- * Request Lenses
    , drOrganizationId
    , drResourceId

    -- * Destructuring the Response
    , describeResourceResponse
    , DescribeResourceResponse
    -- * Response Lenses
    , drrsEmail
    , drrsState
    , drrsResourceId
    , drrsDisabledDate
    , drrsName
    , drrsType
    , drrsEnabledDate
    , drrsBookingOptions
    , drrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'describeResource' smart constructor.
data DescribeResource = DescribeResource'
  { _drOrganizationId :: !Text
  , _drResourceId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drOrganizationId' - The identifier associated with the organization for which the resource is described.
--
-- * 'drResourceId' - The identifier of the resource to be described.
describeResource
    :: Text -- ^ 'drOrganizationId'
    -> Text -- ^ 'drResourceId'
    -> DescribeResource
describeResource pOrganizationId_ pResourceId_ =
  DescribeResource'
    {_drOrganizationId = pOrganizationId_, _drResourceId = pResourceId_}


-- | The identifier associated with the organization for which the resource is described.
drOrganizationId :: Lens' DescribeResource Text
drOrganizationId = lens _drOrganizationId (\ s a -> s{_drOrganizationId = a})

-- | The identifier of the resource to be described.
drResourceId :: Lens' DescribeResource Text
drResourceId = lens _drResourceId (\ s a -> s{_drResourceId = a})

instance AWSRequest DescribeResource where
        type Rs DescribeResource = DescribeResourceResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 DescribeResourceResponse' <$>
                   (x .?> "Email") <*> (x .?> "State") <*>
                     (x .?> "ResourceId")
                     <*> (x .?> "DisabledDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "Type")
                     <*> (x .?> "EnabledDate")
                     <*> (x .?> "BookingOptions")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeResource where

instance NFData DescribeResource where

instance ToHeaders DescribeResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DescribeResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeResource where
        toJSON DescribeResource'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _drOrganizationId),
                  Just ("ResourceId" .= _drResourceId)])

instance ToPath DescribeResource where
        toPath = const "/"

instance ToQuery DescribeResource where
        toQuery = const mempty

-- | /See:/ 'describeResourceResponse' smart constructor.
data DescribeResourceResponse = DescribeResourceResponse'
  { _drrsEmail          :: !(Maybe Text)
  , _drrsState          :: !(Maybe EntityState)
  , _drrsResourceId     :: !(Maybe Text)
  , _drrsDisabledDate   :: !(Maybe POSIX)
  , _drrsName           :: !(Maybe Text)
  , _drrsType           :: !(Maybe ResourceType)
  , _drrsEnabledDate    :: !(Maybe POSIX)
  , _drrsBookingOptions :: !(Maybe BookingOptions)
  , _drrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsEmail' - The email of the described resource.
--
-- * 'drrsState' - The state of the resource: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to Amazon WorkMail).
--
-- * 'drrsResourceId' - The identifier of the described resource.
--
-- * 'drrsDisabledDate' - The date and time when a resource was registered from Amazon WorkMail, in UNIX epoch time format.
--
-- * 'drrsName' - The name of the described resource.
--
-- * 'drrsType' - The type of the described resource.
--
-- * 'drrsEnabledDate' - The date and time when a resource was registered to Amazon WorkMail, in UNIX epoch time format.
--
-- * 'drrsBookingOptions' - The booking options for the described resource.
--
-- * 'drrsResponseStatus' - -- | The response status code.
describeResourceResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeResourceResponse
describeResourceResponse pResponseStatus_ =
  DescribeResourceResponse'
    { _drrsEmail = Nothing
    , _drrsState = Nothing
    , _drrsResourceId = Nothing
    , _drrsDisabledDate = Nothing
    , _drrsName = Nothing
    , _drrsType = Nothing
    , _drrsEnabledDate = Nothing
    , _drrsBookingOptions = Nothing
    , _drrsResponseStatus = pResponseStatus_
    }


-- | The email of the described resource.
drrsEmail :: Lens' DescribeResourceResponse (Maybe Text)
drrsEmail = lens _drrsEmail (\ s a -> s{_drrsEmail = a})

-- | The state of the resource: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to Amazon WorkMail).
drrsState :: Lens' DescribeResourceResponse (Maybe EntityState)
drrsState = lens _drrsState (\ s a -> s{_drrsState = a})

-- | The identifier of the described resource.
drrsResourceId :: Lens' DescribeResourceResponse (Maybe Text)
drrsResourceId = lens _drrsResourceId (\ s a -> s{_drrsResourceId = a})

-- | The date and time when a resource was registered from Amazon WorkMail, in UNIX epoch time format.
drrsDisabledDate :: Lens' DescribeResourceResponse (Maybe UTCTime)
drrsDisabledDate = lens _drrsDisabledDate (\ s a -> s{_drrsDisabledDate = a}) . mapping _Time

-- | The name of the described resource.
drrsName :: Lens' DescribeResourceResponse (Maybe Text)
drrsName = lens _drrsName (\ s a -> s{_drrsName = a})

-- | The type of the described resource.
drrsType :: Lens' DescribeResourceResponse (Maybe ResourceType)
drrsType = lens _drrsType (\ s a -> s{_drrsType = a})

-- | The date and time when a resource was registered to Amazon WorkMail, in UNIX epoch time format.
drrsEnabledDate :: Lens' DescribeResourceResponse (Maybe UTCTime)
drrsEnabledDate = lens _drrsEnabledDate (\ s a -> s{_drrsEnabledDate = a}) . mapping _Time

-- | The booking options for the described resource.
drrsBookingOptions :: Lens' DescribeResourceResponse (Maybe BookingOptions)
drrsBookingOptions = lens _drrsBookingOptions (\ s a -> s{_drrsBookingOptions = a})

-- | -- | The response status code.
drrsResponseStatus :: Lens' DescribeResourceResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a})

instance NFData DescribeResourceResponse where
