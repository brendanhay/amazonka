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
-- Module      : Network.AWS.WorkMail.DescribeOrganization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides more information regarding a given organization based on its identifier.
module Network.AWS.WorkMail.DescribeOrganization
  ( -- * Creating a Request
    describeOrganization,
    DescribeOrganization,

    -- * Request Lenses
    desOrganizationId,

    -- * Destructuring the Response
    describeOrganizationResponse,
    DescribeOrganizationResponse,

    -- * Response Lenses
    dorsDirectoryId,
    dorsState,
    dorsARN,
    dorsAlias,
    dorsCompletedDate,
    dorsDirectoryType,
    dorsDefaultMailDomain,
    dorsErrorMessage,
    dorsOrganizationId,
    dorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'describeOrganization' smart constructor.
newtype DescribeOrganization = DescribeOrganization'
  { _desOrganizationId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desOrganizationId' - The identifier for the organization to be described.
describeOrganization ::
  -- | 'desOrganizationId'
  Text ->
  DescribeOrganization
describeOrganization pOrganizationId_ =
  DescribeOrganization' {_desOrganizationId = pOrganizationId_}

-- | The identifier for the organization to be described.
desOrganizationId :: Lens' DescribeOrganization Text
desOrganizationId = lens _desOrganizationId (\s a -> s {_desOrganizationId = a})

instance AWSRequest DescribeOrganization where
  type Rs DescribeOrganization = DescribeOrganizationResponse
  request = postJSON workMail
  response =
    receiveJSON
      ( \s h x ->
          DescribeOrganizationResponse'
            <$> (x .?> "DirectoryId")
            <*> (x .?> "State")
            <*> (x .?> "ARN")
            <*> (x .?> "Alias")
            <*> (x .?> "CompletedDate")
            <*> (x .?> "DirectoryType")
            <*> (x .?> "DefaultMailDomain")
            <*> (x .?> "ErrorMessage")
            <*> (x .?> "OrganizationId")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeOrganization

instance NFData DescribeOrganization

instance ToHeaders DescribeOrganization where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkMailService.DescribeOrganization" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeOrganization where
  toJSON DescribeOrganization' {..} =
    object
      (catMaybes [Just ("OrganizationId" .= _desOrganizationId)])

instance ToPath DescribeOrganization where
  toPath = const "/"

instance ToQuery DescribeOrganization where
  toQuery = const mempty

-- | /See:/ 'describeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { _dorsDirectoryId ::
      !(Maybe Text),
    _dorsState :: !(Maybe Text),
    _dorsARN :: !(Maybe Text),
    _dorsAlias :: !(Maybe Text),
    _dorsCompletedDate ::
      !(Maybe POSIX),
    _dorsDirectoryType ::
      !(Maybe Text),
    _dorsDefaultMailDomain ::
      !(Maybe Text),
    _dorsErrorMessage ::
      !(Maybe Text),
    _dorsOrganizationId ::
      !(Maybe Text),
    _dorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsDirectoryId' - The identifier for the directory associated with an Amazon WorkMail organization.
--
-- * 'dorsState' - The state of an organization.
--
-- * 'dorsARN' - The Amazon Resource Name (ARN) of the organization.
--
-- * 'dorsAlias' - The alias for an organization.
--
-- * 'dorsCompletedDate' - The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
--
-- * 'dorsDirectoryType' - The type of directory associated with the WorkMail organization.
--
-- * 'dorsDefaultMailDomain' - The default mail domain associated with the organization.
--
-- * 'dorsErrorMessage' - (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
--
-- * 'dorsOrganizationId' - The identifier of an organization.
--
-- * 'dorsResponseStatus' - -- | The response status code.
describeOrganizationResponse ::
  -- | 'dorsResponseStatus'
  Int ->
  DescribeOrganizationResponse
describeOrganizationResponse pResponseStatus_ =
  DescribeOrganizationResponse'
    { _dorsDirectoryId = Nothing,
      _dorsState = Nothing,
      _dorsARN = Nothing,
      _dorsAlias = Nothing,
      _dorsCompletedDate = Nothing,
      _dorsDirectoryType = Nothing,
      _dorsDefaultMailDomain = Nothing,
      _dorsErrorMessage = Nothing,
      _dorsOrganizationId = Nothing,
      _dorsResponseStatus = pResponseStatus_
    }

-- | The identifier for the directory associated with an Amazon WorkMail organization.
dorsDirectoryId :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsDirectoryId = lens _dorsDirectoryId (\s a -> s {_dorsDirectoryId = a})

-- | The state of an organization.
dorsState :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsState = lens _dorsState (\s a -> s {_dorsState = a})

-- | The Amazon Resource Name (ARN) of the organization.
dorsARN :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsARN = lens _dorsARN (\s a -> s {_dorsARN = a})

-- | The alias for an organization.
dorsAlias :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsAlias = lens _dorsAlias (\s a -> s {_dorsAlias = a})

-- | The date at which the organization became usable in the WorkMail context, in UNIX epoch time format.
dorsCompletedDate :: Lens' DescribeOrganizationResponse (Maybe UTCTime)
dorsCompletedDate = lens _dorsCompletedDate (\s a -> s {_dorsCompletedDate = a}) . mapping _Time

-- | The type of directory associated with the WorkMail organization.
dorsDirectoryType :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsDirectoryType = lens _dorsDirectoryType (\s a -> s {_dorsDirectoryType = a})

-- | The default mail domain associated with the organization.
dorsDefaultMailDomain :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsDefaultMailDomain = lens _dorsDefaultMailDomain (\s a -> s {_dorsDefaultMailDomain = a})

-- | (Optional) The error message indicating if unexpected behavior was encountered with regards to the organization.
dorsErrorMessage :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsErrorMessage = lens _dorsErrorMessage (\s a -> s {_dorsErrorMessage = a})

-- | The identifier of an organization.
dorsOrganizationId :: Lens' DescribeOrganizationResponse (Maybe Text)
dorsOrganizationId = lens _dorsOrganizationId (\s a -> s {_dorsOrganizationId = a})

-- | -- | The response status code.
dorsResponseStatus :: Lens' DescribeOrganizationResponse Int
dorsResponseStatus = lens _dorsResponseStatus (\s a -> s {_dorsResponseStatus = a})

instance NFData DescribeOrganizationResponse
