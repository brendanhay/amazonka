{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.OutputLocation where

import Network.AWS.Glacier.Types.S3Location
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the location where the select job results are stored.
--
--
--
-- /See:/ 'outputLocation' smart constructor.
newtype OutputLocation = OutputLocation' {_olS3 :: Maybe S3Location}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olS3' - Describes an S3 location that will receive the results of the job request.
outputLocation ::
  OutputLocation
outputLocation = OutputLocation' {_olS3 = Nothing}

-- | Describes an S3 location that will receive the results of the job request.
olS3 :: Lens' OutputLocation (Maybe S3Location)
olS3 = lens _olS3 (\s a -> s {_olS3 = a})

instance FromJSON OutputLocation where
  parseJSON =
    withObject
      "OutputLocation"
      (\x -> OutputLocation' <$> (x .:? "S3"))

instance Hashable OutputLocation

instance NFData OutputLocation

instance ToJSON OutputLocation where
  toJSON OutputLocation' {..} =
    object (catMaybes [("S3" .=) <$> _olS3])
