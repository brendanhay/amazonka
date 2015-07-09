{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers with AWS CodeDeploy a revision for the specified application.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_RegisterApplicationRevision.html>
module Network.AWS.CodeDeploy.RegisterApplicationRevision
    (
    -- * Request
      RegisterApplicationRevision
    -- ** Request constructor
    , registerApplicationRevision
    -- ** Request lenses
    , rarDescription
    , rarApplicationName
    , rarRevision

    -- * Response
    , RegisterApplicationRevisionResponse
    -- ** Response constructor
    , registerApplicationRevisionResponse
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a register application revision operation.
--
-- /See:/ 'registerApplicationRevision' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rarDescription'
--
-- * 'rarApplicationName'
--
-- * 'rarRevision'
data RegisterApplicationRevision = RegisterApplicationRevision'
    { _rarDescription     :: !(Maybe Text)
    , _rarApplicationName :: !Text
    , _rarRevision        :: !RevisionLocation
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterApplicationRevision' smart constructor.
registerApplicationRevision :: Text -> RevisionLocation -> RegisterApplicationRevision
registerApplicationRevision pApplicationName pRevision =
    RegisterApplicationRevision'
    { _rarDescription = Nothing
    , _rarApplicationName = pApplicationName
    , _rarRevision = pRevision
    }

-- | A comment about the revision.
rarDescription :: Lens' RegisterApplicationRevision (Maybe Text)
rarDescription = lens _rarDescription (\ s a -> s{_rarDescription = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
rarApplicationName :: Lens' RegisterApplicationRevision Text
rarApplicationName = lens _rarApplicationName (\ s a -> s{_rarApplicationName = a});

-- | Information about the application revision to register, including the
-- revision\'s type and its location.
rarRevision :: Lens' RegisterApplicationRevision RevisionLocation
rarRevision = lens _rarRevision (\ s a -> s{_rarRevision = a});

instance AWSRequest RegisterApplicationRevision where
        type Sv RegisterApplicationRevision = CodeDeploy
        type Rs RegisterApplicationRevision =
             RegisterApplicationRevisionResponse
        request = postJSON
        response
          = receiveNull RegisterApplicationRevisionResponse'

instance ToHeaders RegisterApplicationRevision where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.RegisterApplicationRevision" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterApplicationRevision where
        toJSON RegisterApplicationRevision'{..}
          = object
              ["description" .= _rarDescription,
               "applicationName" .= _rarApplicationName,
               "revision" .= _rarRevision]

instance ToPath RegisterApplicationRevision where
        toPath = const "/"

instance ToQuery RegisterApplicationRevision where
        toQuery = const mempty

-- | /See:/ 'registerApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse =
    RegisterApplicationRevisionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterApplicationRevisionResponse' smart constructor.
registerApplicationRevisionResponse :: RegisterApplicationRevisionResponse
registerApplicationRevisionResponse = RegisterApplicationRevisionResponse'
