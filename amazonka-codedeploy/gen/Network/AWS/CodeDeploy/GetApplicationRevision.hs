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
-- Module      : Network.AWS.CodeDeploy.GetApplicationRevision
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application revision.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetApplicationRevision.html AWS API Reference> for GetApplicationRevision.
module Network.AWS.CodeDeploy.GetApplicationRevision
    (
    -- * Creating a Request
      getApplicationRevision
    , GetApplicationRevision
    -- * Request Lenses
    , garApplicationName
    , garRevision

    -- * Destructuring the Response
    , getApplicationRevisionResponse
    , GetApplicationRevisionResponse
    -- * Response Lenses
    , garrsRevisionInfo
    , garrsApplicationName
    , garrsRevision
    , garrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get application revision operation.
--
-- /See:/ 'getApplicationRevision' smart constructor.
data GetApplicationRevision = GetApplicationRevision'
    { _garApplicationName :: !Text
    , _garRevision        :: !RevisionLocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetApplicationRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garApplicationName'
--
-- * 'garRevision'
getApplicationRevision
    :: Text -- ^ 'garApplicationName'
    -> RevisionLocation -- ^ 'garRevision'
    -> GetApplicationRevision
getApplicationRevision pApplicationName_ pRevision_ =
    GetApplicationRevision'
    { _garApplicationName = pApplicationName_
    , _garRevision = pRevision_
    }

-- | The name of the application that corresponds to the revision.
garApplicationName :: Lens' GetApplicationRevision Text
garApplicationName = lens _garApplicationName (\ s a -> s{_garApplicationName = a});

-- | Information about the application revision to get, including the
-- revision\'s type and its location.
garRevision :: Lens' GetApplicationRevision RevisionLocation
garRevision = lens _garRevision (\ s a -> s{_garRevision = a});

instance AWSRequest GetApplicationRevision where
        type Rs GetApplicationRevision =
             GetApplicationRevisionResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 GetApplicationRevisionResponse' <$>
                   (x .?> "revisionInfo") <*> (x .?> "applicationName")
                     <*> (x .?> "revision")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetApplicationRevision where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetApplicationRevision" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetApplicationRevision where
        toJSON GetApplicationRevision'{..}
          = object
              (catMaybes
                 [Just ("applicationName" .= _garApplicationName),
                  Just ("revision" .= _garRevision)])

instance ToPath GetApplicationRevision where
        toPath = const "/"

instance ToQuery GetApplicationRevision where
        toQuery = const mempty

-- | Represents the output of a get application revision operation.
--
-- /See:/ 'getApplicationRevisionResponse' smart constructor.
data GetApplicationRevisionResponse = GetApplicationRevisionResponse'
    { _garrsRevisionInfo    :: !(Maybe GenericRevisionInfo)
    , _garrsApplicationName :: !(Maybe Text)
    , _garrsRevision        :: !(Maybe RevisionLocation)
    , _garrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetApplicationRevisionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garrsRevisionInfo'
--
-- * 'garrsApplicationName'
--
-- * 'garrsRevision'
--
-- * 'garrsStatus'
getApplicationRevisionResponse
    :: Int -- ^ 'garrsStatus'
    -> GetApplicationRevisionResponse
getApplicationRevisionResponse pStatus_ =
    GetApplicationRevisionResponse'
    { _garrsRevisionInfo = Nothing
    , _garrsApplicationName = Nothing
    , _garrsRevision = Nothing
    , _garrsStatus = pStatus_
    }

-- | General information about the revision.
garrsRevisionInfo :: Lens' GetApplicationRevisionResponse (Maybe GenericRevisionInfo)
garrsRevisionInfo = lens _garrsRevisionInfo (\ s a -> s{_garrsRevisionInfo = a});

-- | The name of the application that corresponds to the revision.
garrsApplicationName :: Lens' GetApplicationRevisionResponse (Maybe Text)
garrsApplicationName = lens _garrsApplicationName (\ s a -> s{_garrsApplicationName = a});

-- | Additional information about the revision, including the revision\'s
-- type and its location.
garrsRevision :: Lens' GetApplicationRevisionResponse (Maybe RevisionLocation)
garrsRevision = lens _garrsRevision (\ s a -> s{_garrsRevision = a});

-- | The response status code.
garrsStatus :: Lens' GetApplicationRevisionResponse Int
garrsStatus = lens _garrsStatus (\ s a -> s{_garrsStatus = a});
