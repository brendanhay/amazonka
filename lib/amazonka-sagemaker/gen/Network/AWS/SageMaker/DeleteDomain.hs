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
-- Module      : Network.AWS.SageMaker.DeleteDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to delete a domain. If you onboarded with IAM mode, you will need to delete your domain to onboard again using SSO. Use with caution. All of the members of the domain will lose access to their EFS volume, including data, notebooks, and other artifacts.
module Network.AWS.SageMaker.DeleteDomain
  ( -- * Creating a Request
    deleteDomain,
    DeleteDomain,

    -- * Request Lenses
    dddRetentionPolicy,
    dddDomainId,

    -- * Destructuring the Response
    deleteDomainResponse,
    DeleteDomainResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { _dddRetentionPolicy ::
      !(Maybe RetentionPolicy),
    _dddDomainId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dddRetentionPolicy' - The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted).
--
-- * 'dddDomainId' - The domain ID.
deleteDomain ::
  -- | 'dddDomainId'
  Text ->
  DeleteDomain
deleteDomain pDomainId_ =
  DeleteDomain'
    { _dddRetentionPolicy = Nothing,
      _dddDomainId = pDomainId_
    }

-- | The retention policy for this domain, which specifies whether resources will be retained after the Domain is deleted. By default, all resources are retained (not automatically deleted).
dddRetentionPolicy :: Lens' DeleteDomain (Maybe RetentionPolicy)
dddRetentionPolicy = lens _dddRetentionPolicy (\s a -> s {_dddRetentionPolicy = a})

-- | The domain ID.
dddDomainId :: Lens' DeleteDomain Text
dddDomainId = lens _dddDomainId (\s a -> s {_dddDomainId = a})

instance AWSRequest DeleteDomain where
  type Rs DeleteDomain = DeleteDomainResponse
  request = postJSON sageMaker
  response = receiveNull DeleteDomainResponse'

instance Hashable DeleteDomain

instance NFData DeleteDomain

instance ToHeaders DeleteDomain where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteDomain" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    object
      ( catMaybes
          [ ("RetentionPolicy" .=) <$> _dddRetentionPolicy,
            Just ("DomainId" .= _dddDomainId)
          ]
      )

instance ToPath DeleteDomain where
  toPath = const "/"

instance ToQuery DeleteDomain where
  toQuery = const mempty

-- | /See:/ 'deleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDomainResponse' with the minimum fields required to make a request.
deleteDomainResponse ::
  DeleteDomainResponse
deleteDomainResponse = DeleteDomainResponse'

instance NFData DeleteDomainResponse
