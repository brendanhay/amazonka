{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DomainDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DomainDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.DomainStatus

-- | The domain's details.
--
--
--
-- /See:/ 'domainDetails' smart constructor.
data DomainDetails = DomainDetails'
  { _ddCreationTime ::
      !(Maybe POSIX),
    _ddStatus :: !(Maybe DomainStatus),
    _ddDomainARN :: !(Maybe Text),
    _ddURL :: !(Maybe Text),
    _ddLastModifiedTime :: !(Maybe POSIX),
    _ddDomainName :: !(Maybe Text),
    _ddDomainId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DomainDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddCreationTime' - The creation time.
--
-- * 'ddStatus' - The status.
--
-- * 'ddDomainARN' - The domain's Amazon Resource Name (ARN).
--
-- * 'ddURL' - The domain's URL.
--
-- * 'ddLastModifiedTime' - The last modified time.
--
-- * 'ddDomainName' - The domain name.
--
-- * 'ddDomainId' - The domain ID.
domainDetails ::
  DomainDetails
domainDetails =
  DomainDetails'
    { _ddCreationTime = Nothing,
      _ddStatus = Nothing,
      _ddDomainARN = Nothing,
      _ddURL = Nothing,
      _ddLastModifiedTime = Nothing,
      _ddDomainName = Nothing,
      _ddDomainId = Nothing
    }

-- | The creation time.
ddCreationTime :: Lens' DomainDetails (Maybe UTCTime)
ddCreationTime = lens _ddCreationTime (\s a -> s {_ddCreationTime = a}) . mapping _Time

-- | The status.
ddStatus :: Lens' DomainDetails (Maybe DomainStatus)
ddStatus = lens _ddStatus (\s a -> s {_ddStatus = a})

-- | The domain's Amazon Resource Name (ARN).
ddDomainARN :: Lens' DomainDetails (Maybe Text)
ddDomainARN = lens _ddDomainARN (\s a -> s {_ddDomainARN = a})

-- | The domain's URL.
ddURL :: Lens' DomainDetails (Maybe Text)
ddURL = lens _ddURL (\s a -> s {_ddURL = a})

-- | The last modified time.
ddLastModifiedTime :: Lens' DomainDetails (Maybe UTCTime)
ddLastModifiedTime = lens _ddLastModifiedTime (\s a -> s {_ddLastModifiedTime = a}) . mapping _Time

-- | The domain name.
ddDomainName :: Lens' DomainDetails (Maybe Text)
ddDomainName = lens _ddDomainName (\s a -> s {_ddDomainName = a})

-- | The domain ID.
ddDomainId :: Lens' DomainDetails (Maybe Text)
ddDomainId = lens _ddDomainId (\s a -> s {_ddDomainId = a})

instance FromJSON DomainDetails where
  parseJSON =
    withObject
      "DomainDetails"
      ( \x ->
          DomainDetails'
            <$> (x .:? "CreationTime")
            <*> (x .:? "Status")
            <*> (x .:? "DomainArn")
            <*> (x .:? "Url")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "DomainName")
            <*> (x .:? "DomainId")
      )

instance Hashable DomainDetails

instance NFData DomainDetails
