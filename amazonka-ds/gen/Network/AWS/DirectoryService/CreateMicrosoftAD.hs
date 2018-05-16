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
-- Module      : Network.AWS.DirectoryService.CreateMicrosoftAD
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Microsoft AD in the AWS cloud.
--
--
-- Before you call /CreateMicrosoftAD/ , ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the /CreateMicrosoftAD/ operation, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference> .
--
module Network.AWS.DirectoryService.CreateMicrosoftAD
    (
    -- * Creating a Request
      createMicrosoftAD
    , CreateMicrosoftAD
    -- * Request Lenses
    , cmadEdition
    , cmadShortName
    , cmadDescription
    , cmadName
    , cmadPassword
    , cmadVPCSettings

    -- * Destructuring the Response
    , createMicrosoftADResponse
    , CreateMicrosoftADResponse
    -- * Response Lenses
    , cmadrsDirectoryId
    , cmadrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Creates a Microsoft AD in the AWS cloud.
--
--
--
-- /See:/ 'createMicrosoftAD' smart constructor.
data CreateMicrosoftAD = CreateMicrosoftAD'
  { _cmadEdition     :: !(Maybe DirectoryEdition)
  , _cmadShortName   :: !(Maybe Text)
  , _cmadDescription :: !(Maybe Text)
  , _cmadName        :: !Text
  , _cmadPassword    :: !(Sensitive Text)
  , _cmadVPCSettings :: !DirectoryVPCSettings
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMicrosoftAD' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmadEdition' - AWS Microsoft AD is available in two editions: Standard and Enterprise. Enterprise is the default.
--
-- * 'cmadShortName' - The NetBIOS name for your domain. A short identifier for your domain, such as @CORP@ . If you don't specify a NetBIOS name, it will default to the first part of your directory DNS. For example, @CORP@ for the directory DNS @corp.example.com@ .
--
-- * 'cmadDescription' - A textual description for the directory. This label will appear on the AWS console @Directory Details@ page after the directory is created.
--
-- * 'cmadName' - The fully qualified domain name for the directory, such as @corp.example.com@ . This name will resolve inside your VPC only. It does not need to be publicly resolvable.
--
-- * 'cmadPassword' - The password for the default administrative user named @Admin@ .
--
-- * 'cmadVPCSettings' - Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
createMicrosoftAD
    :: Text -- ^ 'cmadName'
    -> Text -- ^ 'cmadPassword'
    -> DirectoryVPCSettings -- ^ 'cmadVPCSettings'
    -> CreateMicrosoftAD
createMicrosoftAD pName_ pPassword_ pVPCSettings_ =
  CreateMicrosoftAD'
    { _cmadEdition = Nothing
    , _cmadShortName = Nothing
    , _cmadDescription = Nothing
    , _cmadName = pName_
    , _cmadPassword = _Sensitive # pPassword_
    , _cmadVPCSettings = pVPCSettings_
    }


-- | AWS Microsoft AD is available in two editions: Standard and Enterprise. Enterprise is the default.
cmadEdition :: Lens' CreateMicrosoftAD (Maybe DirectoryEdition)
cmadEdition = lens _cmadEdition (\ s a -> s{_cmadEdition = a})

-- | The NetBIOS name for your domain. A short identifier for your domain, such as @CORP@ . If you don't specify a NetBIOS name, it will default to the first part of your directory DNS. For example, @CORP@ for the directory DNS @corp.example.com@ .
cmadShortName :: Lens' CreateMicrosoftAD (Maybe Text)
cmadShortName = lens _cmadShortName (\ s a -> s{_cmadShortName = a})

-- | A textual description for the directory. This label will appear on the AWS console @Directory Details@ page after the directory is created.
cmadDescription :: Lens' CreateMicrosoftAD (Maybe Text)
cmadDescription = lens _cmadDescription (\ s a -> s{_cmadDescription = a})

-- | The fully qualified domain name for the directory, such as @corp.example.com@ . This name will resolve inside your VPC only. It does not need to be publicly resolvable.
cmadName :: Lens' CreateMicrosoftAD Text
cmadName = lens _cmadName (\ s a -> s{_cmadName = a})

-- | The password for the default administrative user named @Admin@ .
cmadPassword :: Lens' CreateMicrosoftAD Text
cmadPassword = lens _cmadPassword (\ s a -> s{_cmadPassword = a}) . _Sensitive

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
cmadVPCSettings :: Lens' CreateMicrosoftAD DirectoryVPCSettings
cmadVPCSettings = lens _cmadVPCSettings (\ s a -> s{_cmadVPCSettings = a})

instance AWSRequest CreateMicrosoftAD where
        type Rs CreateMicrosoftAD = CreateMicrosoftADResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 CreateMicrosoftADResponse' <$>
                   (x .?> "DirectoryId") <*> (pure (fromEnum s)))

instance Hashable CreateMicrosoftAD where

instance NFData CreateMicrosoftAD where

instance ToHeaders CreateMicrosoftAD where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CreateMicrosoftAD" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateMicrosoftAD where
        toJSON CreateMicrosoftAD'{..}
          = object
              (catMaybes
                 [("Edition" .=) <$> _cmadEdition,
                  ("ShortName" .=) <$> _cmadShortName,
                  ("Description" .=) <$> _cmadDescription,
                  Just ("Name" .= _cmadName),
                  Just ("Password" .= _cmadPassword),
                  Just ("VpcSettings" .= _cmadVPCSettings)])

instance ToPath CreateMicrosoftAD where
        toPath = const "/"

instance ToQuery CreateMicrosoftAD where
        toQuery = const mempty

-- | Result of a CreateMicrosoftAD request.
--
--
--
-- /See:/ 'createMicrosoftADResponse' smart constructor.
data CreateMicrosoftADResponse = CreateMicrosoftADResponse'
  { _cmadrsDirectoryId    :: !(Maybe Text)
  , _cmadrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateMicrosoftADResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmadrsDirectoryId' - The identifier of the directory that was created.
--
-- * 'cmadrsResponseStatus' - -- | The response status code.
createMicrosoftADResponse
    :: Int -- ^ 'cmadrsResponseStatus'
    -> CreateMicrosoftADResponse
createMicrosoftADResponse pResponseStatus_ =
  CreateMicrosoftADResponse'
    {_cmadrsDirectoryId = Nothing, _cmadrsResponseStatus = pResponseStatus_}


-- | The identifier of the directory that was created.
cmadrsDirectoryId :: Lens' CreateMicrosoftADResponse (Maybe Text)
cmadrsDirectoryId = lens _cmadrsDirectoryId (\ s a -> s{_cmadrsDirectoryId = a})

-- | -- | The response status code.
cmadrsResponseStatus :: Lens' CreateMicrosoftADResponse Int
cmadrsResponseStatus = lens _cmadrsResponseStatus (\ s a -> s{_cmadrsResponseStatus = a})

instance NFData CreateMicrosoftADResponse where
