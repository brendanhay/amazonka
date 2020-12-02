{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OutputLocation where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.S3Location

-- | Describes the location where the restore job's output is stored.
--
--
--
-- /See:/ 'outputLocation' smart constructor.
newtype OutputLocation = OutputLocation' {_olS3 :: Maybe S3Location}
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olS3' - Describes an S3 location that will receive the results of the restore request.
outputLocation ::
  OutputLocation
outputLocation = OutputLocation' {_olS3 = Nothing}

-- | Describes an S3 location that will receive the results of the restore request.
olS3 :: Lens' OutputLocation (Maybe S3Location)
olS3 = lens _olS3 (\s a -> s {_olS3 = a})

instance Hashable OutputLocation

instance NFData OutputLocation

instance ToXML OutputLocation where
  toXML OutputLocation' {..} = mconcat ["S3" @= _olS3]
