{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Initiator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Initiator where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal

-- | Container element that identifies who initiated the multipart upload.
--
--
--
-- /See:/ 'initiator' smart constructor.
data Initiator = Initiator'
  { _iDisplayName :: !(Maybe Text),
    _iId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Initiator' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iDisplayName' - Name of the Principal.
--
-- * 'iId' - If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
initiator ::
  Initiator
initiator = Initiator' {_iDisplayName = Nothing, _iId = Nothing}

-- | Name of the Principal.
iDisplayName :: Lens' Initiator (Maybe Text)
iDisplayName = lens _iDisplayName (\s a -> s {_iDisplayName = a})

-- | If the principal is an AWS account, it provides the Canonical User ID. If the principal is an IAM User, it provides a user ARN value.
iId :: Lens' Initiator (Maybe Text)
iId = lens _iId (\s a -> s {_iId = a})

instance FromXML Initiator where
  parseXML x = Initiator' <$> (x .@? "DisplayName") <*> (x .@? "ID")

instance Hashable Initiator

instance NFData Initiator
