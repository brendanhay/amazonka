{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.OutputGroupDetail where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.OutputDetail
import Network.AWS.Prelude

-- | Contains details about the output groups specified in the job settings.
--
-- /See:/ 'outputGroupDetail' smart constructor.
newtype OutputGroupDetail = OutputGroupDetail'
  { _ogdOutputDetails ::
      Maybe [OutputDetail]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputGroupDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogdOutputDetails' - Details about the output
outputGroupDetail ::
  OutputGroupDetail
outputGroupDetail = OutputGroupDetail' {_ogdOutputDetails = Nothing}

-- | Details about the output
ogdOutputDetails :: Lens' OutputGroupDetail [OutputDetail]
ogdOutputDetails = lens _ogdOutputDetails (\s a -> s {_ogdOutputDetails = a}) . _Default . _Coerce

instance FromJSON OutputGroupDetail where
  parseJSON =
    withObject
      "OutputGroupDetail"
      (\x -> OutputGroupDetail' <$> (x .:? "outputDetails" .!= mempty))

instance Hashable OutputGroupDetail

instance NFData OutputGroupDetail
