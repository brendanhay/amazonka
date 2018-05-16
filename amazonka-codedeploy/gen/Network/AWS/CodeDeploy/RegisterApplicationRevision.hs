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
-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers with AWS CodeDeploy a revision for the specified application.
--
--
module Network.AWS.CodeDeploy.RegisterApplicationRevision
    (
    -- * Creating a Request
      registerApplicationRevision
    , RegisterApplicationRevision
    -- * Request Lenses
    , rarDescription
    , rarApplicationName
    , rarRevision

    -- * Destructuring the Response
    , registerApplicationRevisionResponse
    , RegisterApplicationRevisionResponse
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a RegisterApplicationRevision operation.
--
--
--
-- /See:/ 'registerApplicationRevision' smart constructor.
data RegisterApplicationRevision = RegisterApplicationRevision'
  { _rarDescription     :: !(Maybe Text)
  , _rarApplicationName :: !Text
  , _rarRevision        :: !RevisionLocation
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterApplicationRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rarDescription' - A comment about the revision.
--
-- * 'rarApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- * 'rarRevision' - Information about the application revision to register, including type and location.
registerApplicationRevision
    :: Text -- ^ 'rarApplicationName'
    -> RevisionLocation -- ^ 'rarRevision'
    -> RegisterApplicationRevision
registerApplicationRevision pApplicationName_ pRevision_ =
  RegisterApplicationRevision'
    { _rarDescription = Nothing
    , _rarApplicationName = pApplicationName_
    , _rarRevision = pRevision_
    }


-- | A comment about the revision.
rarDescription :: Lens' RegisterApplicationRevision (Maybe Text)
rarDescription = lens _rarDescription (\ s a -> s{_rarDescription = a})

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
rarApplicationName :: Lens' RegisterApplicationRevision Text
rarApplicationName = lens _rarApplicationName (\ s a -> s{_rarApplicationName = a})

-- | Information about the application revision to register, including type and location.
rarRevision :: Lens' RegisterApplicationRevision RevisionLocation
rarRevision = lens _rarRevision (\ s a -> s{_rarRevision = a})

instance AWSRequest RegisterApplicationRevision where
        type Rs RegisterApplicationRevision =
             RegisterApplicationRevisionResponse
        request = postJSON codeDeploy
        response
          = receiveNull RegisterApplicationRevisionResponse'

instance Hashable RegisterApplicationRevision where

instance NFData RegisterApplicationRevision where

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
              (catMaybes
                 [("description" .=) <$> _rarDescription,
                  Just ("applicationName" .= _rarApplicationName),
                  Just ("revision" .= _rarRevision)])

instance ToPath RegisterApplicationRevision where
        toPath = const "/"

instance ToQuery RegisterApplicationRevision where
        toQuery = const mempty

-- | /See:/ 'registerApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse =
  RegisterApplicationRevisionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterApplicationRevisionResponse' with the minimum fields required to make a request.
--
registerApplicationRevisionResponse
    :: RegisterApplicationRevisionResponse
registerApplicationRevisionResponse = RegisterApplicationRevisionResponse'


instance NFData RegisterApplicationRevisionResponse
         where
