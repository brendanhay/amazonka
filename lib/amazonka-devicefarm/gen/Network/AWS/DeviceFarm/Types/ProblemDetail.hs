{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ProblemDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ProblemDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a problem detail.
--
--
--
-- /See:/ 'problemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
  { _pdArn :: !(Maybe Text),
    _pdName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProblemDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pdArn' - The problem detail's ARN.
--
-- * 'pdName' - The problem detail's name.
problemDetail ::
  ProblemDetail
problemDetail = ProblemDetail' {_pdArn = Nothing, _pdName = Nothing}

-- | The problem detail's ARN.
pdArn :: Lens' ProblemDetail (Maybe Text)
pdArn = lens _pdArn (\s a -> s {_pdArn = a})

-- | The problem detail's name.
pdName :: Lens' ProblemDetail (Maybe Text)
pdName = lens _pdName (\s a -> s {_pdName = a})

instance FromJSON ProblemDetail where
  parseJSON =
    withObject
      "ProblemDetail"
      (\x -> ProblemDetail' <$> (x .:? "arn") <*> (x .:? "name"))

instance Hashable ProblemDetail

instance NFData ProblemDetail
