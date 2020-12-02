{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxCountRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxCountRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.
--
--
--
-- /See:/ 'maxCountRule' smart constructor.
data MaxCountRule = MaxCountRule'
  { _mcrMaxCount :: !(Maybe Int),
    _mcrDeleteSourceFromS3 :: !(Maybe Bool),
    _mcrEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaxCountRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcrMaxCount' - Specify the maximum number of application versions to retain.
--
-- * 'mcrDeleteSourceFromS3' - Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- * 'mcrEnabled' - Specify @true@ to apply the rule, or @false@ to disable it.
maxCountRule ::
  -- | 'mcrEnabled'
  Bool ->
  MaxCountRule
maxCountRule pEnabled_ =
  MaxCountRule'
    { _mcrMaxCount = Nothing,
      _mcrDeleteSourceFromS3 = Nothing,
      _mcrEnabled = pEnabled_
    }

-- | Specify the maximum number of application versions to retain.
mcrMaxCount :: Lens' MaxCountRule (Maybe Int)
mcrMaxCount = lens _mcrMaxCount (\s a -> s {_mcrMaxCount = a})

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
mcrDeleteSourceFromS3 :: Lens' MaxCountRule (Maybe Bool)
mcrDeleteSourceFromS3 = lens _mcrDeleteSourceFromS3 (\s a -> s {_mcrDeleteSourceFromS3 = a})

-- | Specify @true@ to apply the rule, or @false@ to disable it.
mcrEnabled :: Lens' MaxCountRule Bool
mcrEnabled = lens _mcrEnabled (\s a -> s {_mcrEnabled = a})

instance FromXML MaxCountRule where
  parseXML x =
    MaxCountRule'
      <$> (x .@? "MaxCount")
      <*> (x .@? "DeleteSourceFromS3")
      <*> (x .@ "Enabled")

instance Hashable MaxCountRule

instance NFData MaxCountRule

instance ToQuery MaxCountRule where
  toQuery MaxCountRule' {..} =
    mconcat
      [ "MaxCount" =: _mcrMaxCount,
        "DeleteSourceFromS3" =: _mcrDeleteSourceFromS3,
        "Enabled" =: _mcrEnabled
      ]
