{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.MaxAgeRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.MaxAgeRule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A lifecycle rule that deletes application versions after the specified number of days.
--
--
--
-- /See:/ 'maxAgeRule' smart constructor.
data MaxAgeRule = MaxAgeRule'
  { _marDeleteSourceFromS3 ::
      !(Maybe Bool),
    _marMaxAgeInDays :: !(Maybe Int),
    _marEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaxAgeRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'marDeleteSourceFromS3' - Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
--
-- * 'marMaxAgeInDays' - Specify the number of days to retain an application versions.
--
-- * 'marEnabled' - Specify @true@ to apply the rule, or @false@ to disable it.
maxAgeRule ::
  -- | 'marEnabled'
  Bool ->
  MaxAgeRule
maxAgeRule pEnabled_ =
  MaxAgeRule'
    { _marDeleteSourceFromS3 = Nothing,
      _marMaxAgeInDays = Nothing,
      _marEnabled = pEnabled_
    }

-- | Set to @true@ to delete a version's source bundle from Amazon S3 when Elastic Beanstalk deletes the application version.
marDeleteSourceFromS3 :: Lens' MaxAgeRule (Maybe Bool)
marDeleteSourceFromS3 = lens _marDeleteSourceFromS3 (\s a -> s {_marDeleteSourceFromS3 = a})

-- | Specify the number of days to retain an application versions.
marMaxAgeInDays :: Lens' MaxAgeRule (Maybe Int)
marMaxAgeInDays = lens _marMaxAgeInDays (\s a -> s {_marMaxAgeInDays = a})

-- | Specify @true@ to apply the rule, or @false@ to disable it.
marEnabled :: Lens' MaxAgeRule Bool
marEnabled = lens _marEnabled (\s a -> s {_marEnabled = a})

instance FromXML MaxAgeRule where
  parseXML x =
    MaxAgeRule'
      <$> (x .@? "DeleteSourceFromS3")
      <*> (x .@? "MaxAgeInDays")
      <*> (x .@ "Enabled")

instance Hashable MaxAgeRule

instance NFData MaxAgeRule

instance ToQuery MaxAgeRule where
  toQuery MaxAgeRule' {..} =
    mconcat
      [ "DeleteSourceFromS3" =: _marDeleteSourceFromS3,
        "MaxAgeInDays" =: _marMaxAgeInDays,
        "Enabled" =: _marEnabled
      ]
