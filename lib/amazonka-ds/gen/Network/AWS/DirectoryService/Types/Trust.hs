{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Trust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Trust where

import Network.AWS.DirectoryService.Types.SelectiveAuth
import Network.AWS.DirectoryService.Types.TrustDirection
import Network.AWS.DirectoryService.Types.TrustState
import Network.AWS.DirectoryService.Types.TrustType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a trust relationship between an AWS Managed Microsoft AD directory and an external domain.
--
--
--
-- /See:/ 'trust' smart constructor.
data Trust = Trust'
  { _tDirectoryId :: !(Maybe Text),
    _tTrustState :: !(Maybe TrustState),
    _tLastUpdatedDateTime :: !(Maybe POSIX),
    _tTrustDirection :: !(Maybe TrustDirection),
    _tStateLastUpdatedDateTime :: !(Maybe POSIX),
    _tTrustType :: !(Maybe TrustType),
    _tTrustStateReason :: !(Maybe Text),
    _tSelectiveAuth :: !(Maybe SelectiveAuth),
    _tRemoteDomainName :: !(Maybe Text),
    _tTrustId :: !(Maybe Text),
    _tCreatedDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Trust' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tDirectoryId' - The Directory ID of the AWS directory involved in the trust relationship.
--
-- * 'tTrustState' - The trust relationship state.
--
-- * 'tLastUpdatedDateTime' - The date and time that the trust relationship was last updated.
--
-- * 'tTrustDirection' - The trust relationship direction.
--
-- * 'tStateLastUpdatedDateTime' - The date and time that the TrustState was last updated.
--
-- * 'tTrustType' - The trust relationship type. @Forest@ is the default.
--
-- * 'tTrustStateReason' - The reason for the TrustState.
--
-- * 'tSelectiveAuth' - Current state of selective authentication for the trust.
--
-- * 'tRemoteDomainName' - The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
--
-- * 'tTrustId' - The unique ID of the trust relationship.
--
-- * 'tCreatedDateTime' - The date and time that the trust relationship was created.
trust ::
  Trust
trust =
  Trust'
    { _tDirectoryId = Nothing,
      _tTrustState = Nothing,
      _tLastUpdatedDateTime = Nothing,
      _tTrustDirection = Nothing,
      _tStateLastUpdatedDateTime = Nothing,
      _tTrustType = Nothing,
      _tTrustStateReason = Nothing,
      _tSelectiveAuth = Nothing,
      _tRemoteDomainName = Nothing,
      _tTrustId = Nothing,
      _tCreatedDateTime = Nothing
    }

-- | The Directory ID of the AWS directory involved in the trust relationship.
tDirectoryId :: Lens' Trust (Maybe Text)
tDirectoryId = lens _tDirectoryId (\s a -> s {_tDirectoryId = a})

-- | The trust relationship state.
tTrustState :: Lens' Trust (Maybe TrustState)
tTrustState = lens _tTrustState (\s a -> s {_tTrustState = a})

-- | The date and time that the trust relationship was last updated.
tLastUpdatedDateTime :: Lens' Trust (Maybe UTCTime)
tLastUpdatedDateTime = lens _tLastUpdatedDateTime (\s a -> s {_tLastUpdatedDateTime = a}) . mapping _Time

-- | The trust relationship direction.
tTrustDirection :: Lens' Trust (Maybe TrustDirection)
tTrustDirection = lens _tTrustDirection (\s a -> s {_tTrustDirection = a})

-- | The date and time that the TrustState was last updated.
tStateLastUpdatedDateTime :: Lens' Trust (Maybe UTCTime)
tStateLastUpdatedDateTime = lens _tStateLastUpdatedDateTime (\s a -> s {_tStateLastUpdatedDateTime = a}) . mapping _Time

-- | The trust relationship type. @Forest@ is the default.
tTrustType :: Lens' Trust (Maybe TrustType)
tTrustType = lens _tTrustType (\s a -> s {_tTrustType = a})

-- | The reason for the TrustState.
tTrustStateReason :: Lens' Trust (Maybe Text)
tTrustStateReason = lens _tTrustStateReason (\s a -> s {_tTrustStateReason = a})

-- | Current state of selective authentication for the trust.
tSelectiveAuth :: Lens' Trust (Maybe SelectiveAuth)
tSelectiveAuth = lens _tSelectiveAuth (\s a -> s {_tSelectiveAuth = a})

-- | The Fully Qualified Domain Name (FQDN) of the external domain involved in the trust relationship.
tRemoteDomainName :: Lens' Trust (Maybe Text)
tRemoteDomainName = lens _tRemoteDomainName (\s a -> s {_tRemoteDomainName = a})

-- | The unique ID of the trust relationship.
tTrustId :: Lens' Trust (Maybe Text)
tTrustId = lens _tTrustId (\s a -> s {_tTrustId = a})

-- | The date and time that the trust relationship was created.
tCreatedDateTime :: Lens' Trust (Maybe UTCTime)
tCreatedDateTime = lens _tCreatedDateTime (\s a -> s {_tCreatedDateTime = a}) . mapping _Time

instance FromJSON Trust where
  parseJSON =
    withObject
      "Trust"
      ( \x ->
          Trust'
            <$> (x .:? "DirectoryId")
            <*> (x .:? "TrustState")
            <*> (x .:? "LastUpdatedDateTime")
            <*> (x .:? "TrustDirection")
            <*> (x .:? "StateLastUpdatedDateTime")
            <*> (x .:? "TrustType")
            <*> (x .:? "TrustStateReason")
            <*> (x .:? "SelectiveAuth")
            <*> (x .:? "RemoteDomainName")
            <*> (x .:? "TrustId")
            <*> (x .:? "CreatedDateTime")
      )

instance Hashable Trust

instance NFData Trust
