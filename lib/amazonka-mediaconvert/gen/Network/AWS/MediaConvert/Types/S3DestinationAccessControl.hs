{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.S3DestinationAccessControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.S3DestinationAccessControl where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.S3ObjectCannedACL
import Network.AWS.Prelude

-- | Optional. Have MediaConvert automatically apply Amazon S3 access control for the outputs in this output group. When you don't use this setting, S3 automatically applies the default access control list PRIVATE.
--
-- /See:/ 's3DestinationAccessControl' smart constructor.
newtype S3DestinationAccessControl = S3DestinationAccessControl'
  { _sdacCannedACL ::
      Maybe S3ObjectCannedACL
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3DestinationAccessControl' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdacCannedACL' - Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
s3DestinationAccessControl ::
  S3DestinationAccessControl
s3DestinationAccessControl =
  S3DestinationAccessControl' {_sdacCannedACL = Nothing}

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
sdacCannedACL :: Lens' S3DestinationAccessControl (Maybe S3ObjectCannedACL)
sdacCannedACL = lens _sdacCannedACL (\s a -> s {_sdacCannedACL = a})

instance FromJSON S3DestinationAccessControl where
  parseJSON =
    withObject
      "S3DestinationAccessControl"
      (\x -> S3DestinationAccessControl' <$> (x .:? "cannedAcl"))

instance Hashable S3DestinationAccessControl

instance NFData S3DestinationAccessControl

instance ToJSON S3DestinationAccessControl where
  toJSON S3DestinationAccessControl' {..} =
    object (catMaybes [("cannedAcl" .=) <$> _sdacCannedACL])
