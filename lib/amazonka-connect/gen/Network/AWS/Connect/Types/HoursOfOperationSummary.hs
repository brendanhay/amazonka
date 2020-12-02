{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.HoursOfOperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.HoursOfOperationSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains summary information about hours of operation for a contact center.
--
--
--
-- /See:/ 'hoursOfOperationSummary' smart constructor.
data HoursOfOperationSummary = HoursOfOperationSummary'
  { _hoosARN ::
      !(Maybe Text),
    _hoosName :: !(Maybe Text),
    _hoosId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HoursOfOperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hoosARN' - The Amazon Resource Name (ARN) of the hours of operation.
--
-- * 'hoosName' - The name of the hours of operation.
--
-- * 'hoosId' - The identifier of the hours of operation.
hoursOfOperationSummary ::
  HoursOfOperationSummary
hoursOfOperationSummary =
  HoursOfOperationSummary'
    { _hoosARN = Nothing,
      _hoosName = Nothing,
      _hoosId = Nothing
    }

-- | The Amazon Resource Name (ARN) of the hours of operation.
hoosARN :: Lens' HoursOfOperationSummary (Maybe Text)
hoosARN = lens _hoosARN (\s a -> s {_hoosARN = a})

-- | The name of the hours of operation.
hoosName :: Lens' HoursOfOperationSummary (Maybe Text)
hoosName = lens _hoosName (\s a -> s {_hoosName = a})

-- | The identifier of the hours of operation.
hoosId :: Lens' HoursOfOperationSummary (Maybe Text)
hoosId = lens _hoosId (\s a -> s {_hoosId = a})

instance FromJSON HoursOfOperationSummary where
  parseJSON =
    withObject
      "HoursOfOperationSummary"
      ( \x ->
          HoursOfOperationSummary'
            <$> (x .:? "Arn") <*> (x .:? "Name") <*> (x .:? "Id")
      )

instance Hashable HoursOfOperationSummary

instance NFData HoursOfOperationSummary
