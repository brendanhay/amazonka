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
-- Module      : Network.AWS.WorkMail.CreateOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail organization. Optionally, you can choose to associate an existing AWS Directory Service directory with your organization. If an AWS Directory Service directory ID is specified, the organization alias must match the directory alias. If you choose not to associate an existing directory with your organization, then we create a new Amazon WorkMail directory for you. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_new_organization.html Adding an organization> in the /Amazon WorkMail Administrator Guide/ .
--
--
-- You can associate multiple email domains with an organization, then set your default email domain from the Amazon WorkMail console. You can also associate a domain that is managed in an Amazon Route 53 public hosted zone. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain> and <https://docs.aws.amazon.com/workmail/latest/adminguide/default_domain.html Choosing the default domain> in the /Amazon WorkMail Administrator Guide/ .
--
-- Optionally, you can use a customer managed master key from AWS Key Management Service (AWS KMS) to encrypt email for your organization. If you don't associate an AWS KMS key, Amazon WorkMail creates a default AWS managed master key for you.
module Network.AWS.WorkMail.CreateOrganization
  ( -- * Creating a Request
    createOrganization,
    CreateOrganization,

    -- * Request Lenses
    coDirectoryId,
    coEnableInteroperability,
    coKMSKeyARN,
    coClientToken,
    coDomains,
    coAlias,

    -- * Destructuring the Response
    createOrganizationResponse,
    CreateOrganizationResponse,

    -- * Response Lenses
    corsOrganizationId,
    corsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'createOrganization' smart constructor.
data CreateOrganization = CreateOrganization'
  { _coDirectoryId ::
      !(Maybe Text),
    _coEnableInteroperability :: !(Maybe Bool),
    _coKMSKeyARN :: !(Maybe Text),
    _coClientToken :: !(Maybe Text),
    _coDomains :: !(Maybe [Domain]),
    _coAlias :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coDirectoryId' - The AWS Directory Service directory ID.
--
-- * 'coEnableInteroperability' - When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
--
-- * 'coKMSKeyARN' - The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
--
-- * 'coClientToken' - The idempotency token associated with the request.
--
-- * 'coDomains' - The email domains to associate with the organization.
--
-- * 'coAlias' - The organization alias.
createOrganization ::
  -- | 'coAlias'
  Text ->
  CreateOrganization
createOrganization pAlias_ =
  CreateOrganization'
    { _coDirectoryId = Nothing,
      _coEnableInteroperability = Nothing,
      _coKMSKeyARN = Nothing,
      _coClientToken = Nothing,
      _coDomains = Nothing,
      _coAlias = pAlias_
    }

-- | The AWS Directory Service directory ID.
coDirectoryId :: Lens' CreateOrganization (Maybe Text)
coDirectoryId = lens _coDirectoryId (\s a -> s {_coDirectoryId = a})

-- | When @true@ , allows organization interoperability between Amazon WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD Connector directory ID is included in the request.
coEnableInteroperability :: Lens' CreateOrganization (Maybe Bool)
coEnableInteroperability = lens _coEnableInteroperability (\s a -> s {_coEnableInteroperability = a})

-- | The Amazon Resource Name (ARN) of a customer managed master key from AWS KMS.
coKMSKeyARN :: Lens' CreateOrganization (Maybe Text)
coKMSKeyARN = lens _coKMSKeyARN (\s a -> s {_coKMSKeyARN = a})

-- | The idempotency token associated with the request.
coClientToken :: Lens' CreateOrganization (Maybe Text)
coClientToken = lens _coClientToken (\s a -> s {_coClientToken = a})

-- | The email domains to associate with the organization.
coDomains :: Lens' CreateOrganization [Domain]
coDomains = lens _coDomains (\s a -> s {_coDomains = a}) . _Default . _Coerce

-- | The organization alias.
coAlias :: Lens' CreateOrganization Text
coAlias = lens _coAlias (\s a -> s {_coAlias = a})

instance AWSRequest CreateOrganization where
  type Rs CreateOrganization = CreateOrganizationResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          CreateOrganizationResponse'
            <$> (x .?> "OrganizationId") <*> (pure (fromEnum s))
      )

instance Hashable CreateOrganization

instance NFData CreateOrganization

instance ToHeaders CreateOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.CreateOrganization" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateOrganization where
  toJSON CreateOrganization' {..} =
    object
      ( catMaybes
          [ ("DirectoryId" .=) <$> _coDirectoryId,
            ("EnableInteroperability" .=) <$> _coEnableInteroperability,
            ("KmsKeyArn" .=) <$> _coKMSKeyARN,
            ("ClientToken" .=) <$> _coClientToken,
            ("Domains" .=) <$> _coDomains,
            Just ("Alias" .= _coAlias)
          ]
      )

instance ToPath CreateOrganization where
  toPath = const "/"

instance ToQuery CreateOrganization where
  toQuery = const mempty

-- | /See:/ 'createOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { _corsOrganizationId ::
      !(Maybe Text),
    _corsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corsOrganizationId' - The organization ID.
--
-- * 'corsResponseStatus' - -- | The response status code.
createOrganizationResponse ::
  -- | 'corsResponseStatus'
  Int ->
  CreateOrganizationResponse
createOrganizationResponse pResponseStatus_ =
  CreateOrganizationResponse'
    { _corsOrganizationId = Nothing,
      _corsResponseStatus = pResponseStatus_
    }

-- | The organization ID.
corsOrganizationId :: Lens' CreateOrganizationResponse (Maybe Text)
corsOrganizationId = lens _corsOrganizationId (\s a -> s {_corsOrganizationId = a})

-- | -- | The response status code.
corsResponseStatus :: Lens' CreateOrganizationResponse Int
corsResponseStatus = lens _corsResponseStatus (\s a -> s {_corsResponseStatus = a})

instance NFData CreateOrganizationResponse
