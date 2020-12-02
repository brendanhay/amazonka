{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.SupportService
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.SupportService where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Support.Types.Category

-- | Information about an AWS service returned by the 'DescribeServices' operation.
--
--
--
-- /See:/ 'supportService' smart constructor.
data SupportService = SupportService'
  { _ssCategories ::
      !(Maybe [Category]),
    _ssName :: !(Maybe Text),
    _ssCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SupportService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssCategories' - A list of categories that describe the type of support issue a case describes. Categories consist of a category name and a category code. Category names and codes are passed to AWS Support when you call 'CreateCase' .
--
-- * 'ssName' - The friendly name for an AWS service. The @code@ element contains the corresponding code.
--
-- * 'ssCode' - The code for an AWS service returned by the 'DescribeServices' response. The @name@ element contains the corresponding friendly name.
supportService ::
  SupportService
supportService =
  SupportService'
    { _ssCategories = Nothing,
      _ssName = Nothing,
      _ssCode = Nothing
    }

-- | A list of categories that describe the type of support issue a case describes. Categories consist of a category name and a category code. Category names and codes are passed to AWS Support when you call 'CreateCase' .
ssCategories :: Lens' SupportService [Category]
ssCategories = lens _ssCategories (\s a -> s {_ssCategories = a}) . _Default . _Coerce

-- | The friendly name for an AWS service. The @code@ element contains the corresponding code.
ssName :: Lens' SupportService (Maybe Text)
ssName = lens _ssName (\s a -> s {_ssName = a})

-- | The code for an AWS service returned by the 'DescribeServices' response. The @name@ element contains the corresponding friendly name.
ssCode :: Lens' SupportService (Maybe Text)
ssCode = lens _ssCode (\s a -> s {_ssCode = a})

instance FromJSON SupportService where
  parseJSON =
    withObject
      "SupportService"
      ( \x ->
          SupportService'
            <$> (x .:? "categories" .!= mempty)
            <*> (x .:? "name")
            <*> (x .:? "code")
      )

instance Hashable SupportService

instance NFData SupportService
