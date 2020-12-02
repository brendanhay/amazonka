{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.UserData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.UserData where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.S3Location

-- | A script that runs on first launch of an Amazon EC2 instance. Used for configuring the server during launch.
--
--
--
-- /See:/ 'userData' smart constructor.
newtype UserData = UserData' {_udS3Location :: Maybe S3Location}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udS3Location' - Amazon S3 location of the user-data script.
userData ::
  UserData
userData = UserData' {_udS3Location = Nothing}

-- | Amazon S3 location of the user-data script.
udS3Location :: Lens' UserData (Maybe S3Location)
udS3Location = lens _udS3Location (\s a -> s {_udS3Location = a})

instance FromJSON UserData where
  parseJSON =
    withObject "UserData" (\x -> UserData' <$> (x .:? "s3Location"))

instance Hashable UserData

instance NFData UserData

instance ToJSON UserData where
  toJSON UserData' {..} =
    object (catMaybes [("s3Location" .=) <$> _udS3Location])
