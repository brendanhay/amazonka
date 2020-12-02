{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.S3OutputURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputURL where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A URL for the S3 bucket where you want to store the results of this request.
--
--
--
-- /See:/ 's3OutputURL' smart constructor.
newtype S3OutputURL = S3OutputURL' {_souOutputURL :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3OutputURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'souOutputURL' - A URL for an S3 bucket where you want to store the results of this request.
s3OutputURL ::
  S3OutputURL
s3OutputURL = S3OutputURL' {_souOutputURL = Nothing}

-- | A URL for an S3 bucket where you want to store the results of this request.
souOutputURL :: Lens' S3OutputURL (Maybe Text)
souOutputURL = lens _souOutputURL (\s a -> s {_souOutputURL = a})

instance FromJSON S3OutputURL where
  parseJSON =
    withObject
      "S3OutputURL"
      (\x -> S3OutputURL' <$> (x .:? "OutputUrl"))

instance Hashable S3OutputURL

instance NFData S3OutputURL
