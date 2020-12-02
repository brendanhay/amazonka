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
-- Module      : Network.AWS.DirectoryService.DescribeLDAPSSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of LDAP security for the specified directory.
module Network.AWS.DirectoryService.DescribeLDAPSSettings
  ( -- * Creating a Request
    describeLDAPSSettings,
    DescribeLDAPSSettings,

    -- * Request Lenses
    dldapssNextToken,
    dldapssLimit,
    dldapssType,
    dldapssDirectoryId,

    -- * Destructuring the Response
    describeLDAPSSettingsResponse,
    DescribeLDAPSSettingsResponse,

    -- * Response Lenses
    dldapssrsLDAPSSettingsInfo,
    dldapssrsNextToken,
    dldapssrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLDAPSSettings' smart constructor.
data DescribeLDAPSSettings = DescribeLDAPSSettings'
  { _dldapssNextToken ::
      !(Maybe Text),
    _dldapssLimit :: !(Maybe Nat),
    _dldapssType :: !(Maybe LDAPSType),
    _dldapssDirectoryId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLDAPSSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldapssNextToken' - The type of next token used for pagination.
--
-- * 'dldapssLimit' - Specifies the number of items that should be displayed on one page.
--
-- * 'dldapssType' - The type of LDAP security to enable. Currently only the value @Client@ is supported.
--
-- * 'dldapssDirectoryId' - The identifier of the directory.
describeLDAPSSettings ::
  -- | 'dldapssDirectoryId'
  Text ->
  DescribeLDAPSSettings
describeLDAPSSettings pDirectoryId_ =
  DescribeLDAPSSettings'
    { _dldapssNextToken = Nothing,
      _dldapssLimit = Nothing,
      _dldapssType = Nothing,
      _dldapssDirectoryId = pDirectoryId_
    }

-- | The type of next token used for pagination.
dldapssNextToken :: Lens' DescribeLDAPSSettings (Maybe Text)
dldapssNextToken = lens _dldapssNextToken (\s a -> s {_dldapssNextToken = a})

-- | Specifies the number of items that should be displayed on one page.
dldapssLimit :: Lens' DescribeLDAPSSettings (Maybe Natural)
dldapssLimit = lens _dldapssLimit (\s a -> s {_dldapssLimit = a}) . mapping _Nat

-- | The type of LDAP security to enable. Currently only the value @Client@ is supported.
dldapssType :: Lens' DescribeLDAPSSettings (Maybe LDAPSType)
dldapssType = lens _dldapssType (\s a -> s {_dldapssType = a})

-- | The identifier of the directory.
dldapssDirectoryId :: Lens' DescribeLDAPSSettings Text
dldapssDirectoryId = lens _dldapssDirectoryId (\s a -> s {_dldapssDirectoryId = a})

instance AWSRequest DescribeLDAPSSettings where
  type Rs DescribeLDAPSSettings = DescribeLDAPSSettingsResponse
  request = postJSON directoryService
  response =
    receiveJSON
      ( \s h x ->
          DescribeLDAPSSettingsResponse'
            <$> (x .?> "LDAPSSettingsInfo" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeLDAPSSettings

instance NFData DescribeLDAPSSettings

instance ToHeaders DescribeLDAPSSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DirectoryService_20150416.DescribeLDAPSSettings" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeLDAPSSettings where
  toJSON DescribeLDAPSSettings' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _dldapssNextToken,
            ("Limit" .=) <$> _dldapssLimit,
            ("Type" .=) <$> _dldapssType,
            Just ("DirectoryId" .= _dldapssDirectoryId)
          ]
      )

instance ToPath DescribeLDAPSSettings where
  toPath = const "/"

instance ToQuery DescribeLDAPSSettings where
  toQuery = const mempty

-- | /See:/ 'describeLDAPSSettingsResponse' smart constructor.
data DescribeLDAPSSettingsResponse = DescribeLDAPSSettingsResponse'
  { _dldapssrsLDAPSSettingsInfo ::
      !(Maybe [LDAPSSettingInfo]),
    _dldapssrsNextToken ::
      !(Maybe Text),
    _dldapssrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeLDAPSSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldapssrsLDAPSSettingsInfo' - Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
--
-- * 'dldapssrsNextToken' - The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
--
-- * 'dldapssrsResponseStatus' - -- | The response status code.
describeLDAPSSettingsResponse ::
  -- | 'dldapssrsResponseStatus'
  Int ->
  DescribeLDAPSSettingsResponse
describeLDAPSSettingsResponse pResponseStatus_ =
  DescribeLDAPSSettingsResponse'
    { _dldapssrsLDAPSSettingsInfo =
        Nothing,
      _dldapssrsNextToken = Nothing,
      _dldapssrsResponseStatus = pResponseStatus_
    }

-- | Information about LDAP security for the specified directory, including status of enablement, state last updated date time, and the reason for the state.
dldapssrsLDAPSSettingsInfo :: Lens' DescribeLDAPSSettingsResponse [LDAPSSettingInfo]
dldapssrsLDAPSSettingsInfo = lens _dldapssrsLDAPSSettingsInfo (\s a -> s {_dldapssrsLDAPSSettingsInfo = a}) . _Default . _Coerce

-- | The next token used to retrieve the LDAPS settings if the number of setting types exceeds page limit and there is another page.
dldapssrsNextToken :: Lens' DescribeLDAPSSettingsResponse (Maybe Text)
dldapssrsNextToken = lens _dldapssrsNextToken (\s a -> s {_dldapssrsNextToken = a})

-- | -- | The response status code.
dldapssrsResponseStatus :: Lens' DescribeLDAPSSettingsResponse Int
dldapssrsResponseStatus = lens _dldapssrsResponseStatus (\s a -> s {_dldapssrsResponseStatus = a})

instance NFData DescribeLDAPSSettingsResponse
