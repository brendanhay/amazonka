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
-- Module      : Network.AWS.ECR.PutImageTagMutability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image tag mutability settings for the specified repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-tag-mutability.html Image Tag Mutability> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.PutImageTagMutability
  ( -- * Creating a Request
    putImageTagMutability,
    PutImageTagMutability,

    -- * Request Lenses
    pitmRegistryId,
    pitmRepositoryName,
    pitmImageTagMutability,

    -- * Destructuring the Response
    putImageTagMutabilityResponse,
    PutImageTagMutabilityResponse,

    -- * Response Lenses
    pitmrsRegistryId,
    pitmrsRepositoryName,
    pitmrsImageTagMutability,
    pitmrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putImageTagMutability' smart constructor.
data PutImageTagMutability = PutImageTagMutability'
  { _pitmRegistryId ::
      !(Maybe Text),
    _pitmRepositoryName :: !Text,
    _pitmImageTagMutability :: !ImageTagMutability
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutImageTagMutability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pitmRegistryId' - The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
--
-- * 'pitmRepositoryName' - The name of the repository in which to update the image tag mutability settings.
--
-- * 'pitmImageTagMutability' - The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
putImageTagMutability ::
  -- | 'pitmRepositoryName'
  Text ->
  -- | 'pitmImageTagMutability'
  ImageTagMutability ->
  PutImageTagMutability
putImageTagMutability pRepositoryName_ pImageTagMutability_ =
  PutImageTagMutability'
    { _pitmRegistryId = Nothing,
      _pitmRepositoryName = pRepositoryName_,
      _pitmImageTagMutability = pImageTagMutability_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to update the image tag mutability settings. If you do not specify a registry, the default registry is assumed.
pitmRegistryId :: Lens' PutImageTagMutability (Maybe Text)
pitmRegistryId = lens _pitmRegistryId (\s a -> s {_pitmRegistryId = a})

-- | The name of the repository in which to update the image tag mutability settings.
pitmRepositoryName :: Lens' PutImageTagMutability Text
pitmRepositoryName = lens _pitmRepositoryName (\s a -> s {_pitmRepositoryName = a})

-- | The tag mutability setting for the repository. If @MUTABLE@ is specified, image tags can be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
pitmImageTagMutability :: Lens' PutImageTagMutability ImageTagMutability
pitmImageTagMutability = lens _pitmImageTagMutability (\s a -> s {_pitmImageTagMutability = a})

instance AWSRequest PutImageTagMutability where
  type Rs PutImageTagMutability = PutImageTagMutabilityResponse
  request = postJSON ecr
  response =
    receiveJSON
      ( \s h x ->
          PutImageTagMutabilityResponse'
            <$> (x .?> "registryId")
            <*> (x .?> "repositoryName")
            <*> (x .?> "imageTagMutability")
            <*> (pure (fromEnum s))
      )

instance Hashable PutImageTagMutability

instance NFData PutImageTagMutability

instance ToHeaders PutImageTagMutability where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerRegistry_V20150921.PutImageTagMutability" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutImageTagMutability where
  toJSON PutImageTagMutability' {..} =
    object
      ( catMaybes
          [ ("registryId" .=) <$> _pitmRegistryId,
            Just ("repositoryName" .= _pitmRepositoryName),
            Just ("imageTagMutability" .= _pitmImageTagMutability)
          ]
      )

instance ToPath PutImageTagMutability where
  toPath = const "/"

instance ToQuery PutImageTagMutability where
  toQuery = const mempty

-- | /See:/ 'putImageTagMutabilityResponse' smart constructor.
data PutImageTagMutabilityResponse = PutImageTagMutabilityResponse'
  { _pitmrsRegistryId ::
      !(Maybe Text),
    _pitmrsRepositoryName ::
      !(Maybe Text),
    _pitmrsImageTagMutability ::
      !(Maybe ImageTagMutability),
    _pitmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutImageTagMutabilityResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pitmrsRegistryId' - The registry ID associated with the request.
--
-- * 'pitmrsRepositoryName' - The repository name associated with the request.
--
-- * 'pitmrsImageTagMutability' - The image tag mutability setting for the repository.
--
-- * 'pitmrsResponseStatus' - -- | The response status code.
putImageTagMutabilityResponse ::
  -- | 'pitmrsResponseStatus'
  Int ->
  PutImageTagMutabilityResponse
putImageTagMutabilityResponse pResponseStatus_ =
  PutImageTagMutabilityResponse'
    { _pitmrsRegistryId = Nothing,
      _pitmrsRepositoryName = Nothing,
      _pitmrsImageTagMutability = Nothing,
      _pitmrsResponseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
pitmrsRegistryId :: Lens' PutImageTagMutabilityResponse (Maybe Text)
pitmrsRegistryId = lens _pitmrsRegistryId (\s a -> s {_pitmrsRegistryId = a})

-- | The repository name associated with the request.
pitmrsRepositoryName :: Lens' PutImageTagMutabilityResponse (Maybe Text)
pitmrsRepositoryName = lens _pitmrsRepositoryName (\s a -> s {_pitmrsRepositoryName = a})

-- | The image tag mutability setting for the repository.
pitmrsImageTagMutability :: Lens' PutImageTagMutabilityResponse (Maybe ImageTagMutability)
pitmrsImageTagMutability = lens _pitmrsImageTagMutability (\s a -> s {_pitmrsImageTagMutability = a})

-- | -- | The response status code.
pitmrsResponseStatus :: Lens' PutImageTagMutabilityResponse Int
pitmrsResponseStatus = lens _pitmrsResponseStatus (\s a -> s {_pitmrsResponseStatus = a})

instance NFData PutImageTagMutabilityResponse
