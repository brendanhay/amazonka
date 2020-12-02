{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.PutFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon EFS @FileSystemPolicy@ to an Amazon EFS file system. A file system policy is an IAM resource-based policy and can contain multiple policy statements. A file system always has exactly one file system policy, which can be the default policy or an explicit policy set or updated using this API operation. When an explicit policy is set, it overrides the default policy. For more information about the default file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/iam-access-control-nfs-efs.html#default-filesystempolicy Default EFS File System Policy> .
--
--
-- This operation requires permissions for the @elasticfilesystem:PutFileSystemPolicy@ action.
module Network.AWS.EFS.PutFileSystemPolicy
  ( -- * Creating a Request
    putFileSystemPolicy,
    PutFileSystemPolicy,

    -- * Request Lenses
    pfspBypassPolicyLockoutSafetyCheck,
    pfspFileSystemId,
    pfspPolicy,

    -- * Destructuring the Response
    fileSystemPolicyDescription,
    FileSystemPolicyDescription,

    -- * Response Lenses
    fspdFileSystemId,
    fspdPolicy,
  )
where

import Network.AWS.EFS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putFileSystemPolicy' smart constructor.
data PutFileSystemPolicy = PutFileSystemPolicy'
  { _pfspBypassPolicyLockoutSafetyCheck ::
      !(Maybe Bool),
    _pfspFileSystemId :: !Text,
    _pfspPolicy :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutFileSystemPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfspBypassPolicyLockoutSafetyCheck' - (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@ lockout safety check. The policy lockout safety check determines whether the policy in the request will prevent the principal making the request will be locked out from making future @PutFileSystemPolicy@ requests on the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to prevent the principal that is making the request from making a subsequent @PutFileSystemPolicy@ request on the file system. The default value is False.
--
-- * 'pfspFileSystemId' - The ID of the EFS file system that you want to create or update the @FileSystemPolicy@ for.
--
-- * 'pfspPolicy' - The @FileSystemPolicy@ that you're creating. Accepts a JSON formatted policy definition. To find out more about the elements that make up a file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies> .
putFileSystemPolicy ::
  -- | 'pfspFileSystemId'
  Text ->
  -- | 'pfspPolicy'
  Text ->
  PutFileSystemPolicy
putFileSystemPolicy pFileSystemId_ pPolicy_ =
  PutFileSystemPolicy'
    { _pfspBypassPolicyLockoutSafetyCheck =
        Nothing,
      _pfspFileSystemId = pFileSystemId_,
      _pfspPolicy = pPolicy_
    }

-- | (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@ lockout safety check. The policy lockout safety check determines whether the policy in the request will prevent the principal making the request will be locked out from making future @PutFileSystemPolicy@ requests on the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to prevent the principal that is making the request from making a subsequent @PutFileSystemPolicy@ request on the file system. The default value is False.
pfspBypassPolicyLockoutSafetyCheck :: Lens' PutFileSystemPolicy (Maybe Bool)
pfspBypassPolicyLockoutSafetyCheck = lens _pfspBypassPolicyLockoutSafetyCheck (\s a -> s {_pfspBypassPolicyLockoutSafetyCheck = a})

-- | The ID of the EFS file system that you want to create or update the @FileSystemPolicy@ for.
pfspFileSystemId :: Lens' PutFileSystemPolicy Text
pfspFileSystemId = lens _pfspFileSystemId (\s a -> s {_pfspFileSystemId = a})

-- | The @FileSystemPolicy@ that you're creating. Accepts a JSON formatted policy definition. To find out more about the elements that make up a file system policy, see <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies> .
pfspPolicy :: Lens' PutFileSystemPolicy Text
pfspPolicy = lens _pfspPolicy (\s a -> s {_pfspPolicy = a})

instance AWSRequest PutFileSystemPolicy where
  type Rs PutFileSystemPolicy = FileSystemPolicyDescription
  request = putJSON efs
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable PutFileSystemPolicy

instance NFData PutFileSystemPolicy

instance ToHeaders PutFileSystemPolicy where
  toHeaders = const mempty

instance ToJSON PutFileSystemPolicy where
  toJSON PutFileSystemPolicy' {..} =
    object
      ( catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" .=)
              <$> _pfspBypassPolicyLockoutSafetyCheck,
            Just ("Policy" .= _pfspPolicy)
          ]
      )

instance ToPath PutFileSystemPolicy where
  toPath PutFileSystemPolicy' {..} =
    mconcat
      ["/2015-02-01/file-systems/", toBS _pfspFileSystemId, "/policy"]

instance ToQuery PutFileSystemPolicy where
  toQuery = const mempty
