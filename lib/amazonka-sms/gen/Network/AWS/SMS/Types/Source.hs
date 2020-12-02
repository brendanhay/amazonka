{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Source
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Source where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.S3Location

-- | Contains the location of a validation script.
--
--
--
-- /See:/ 'source' smart constructor.
newtype Source = Source' {_sS3Location :: Maybe S3Location}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Source' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sS3Location' - Undocumented member.
source ::
  Source
source = Source' {_sS3Location = Nothing}

-- | Undocumented member.
sS3Location :: Lens' Source (Maybe S3Location)
sS3Location = lens _sS3Location (\s a -> s {_sS3Location = a})

instance FromJSON Source where
  parseJSON =
    withObject "Source" (\x -> Source' <$> (x .:? "s3Location"))

instance Hashable Source

instance NFData Source

instance ToJSON Source where
  toJSON Source' {..} =
    object (catMaybes [("s3Location" .=) <$> _sS3Location])
