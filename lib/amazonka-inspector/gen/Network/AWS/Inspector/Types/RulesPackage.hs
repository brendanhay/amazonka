{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.RulesPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.RulesPackage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an Amazon Inspector rules package. This data type is used as the response element in the 'DescribeRulesPackages' action.
--
--
--
-- /See:/ 'rulesPackage' smart constructor.
data RulesPackage = RulesPackage'
  { _rpDescription :: !(Maybe Text),
    _rpArn :: !Text,
    _rpName :: !Text,
    _rpVersion :: !Text,
    _rpProvider :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RulesPackage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpDescription' - The description of the rules package.
--
-- * 'rpArn' - The ARN of the rules package.
--
-- * 'rpName' - The name of the rules package.
--
-- * 'rpVersion' - The version ID of the rules package.
--
-- * 'rpProvider' - The provider of the rules package.
rulesPackage ::
  -- | 'rpArn'
  Text ->
  -- | 'rpName'
  Text ->
  -- | 'rpVersion'
  Text ->
  -- | 'rpProvider'
  Text ->
  RulesPackage
rulesPackage pArn_ pName_ pVersion_ pProvider_ =
  RulesPackage'
    { _rpDescription = Nothing,
      _rpArn = pArn_,
      _rpName = pName_,
      _rpVersion = pVersion_,
      _rpProvider = pProvider_
    }

-- | The description of the rules package.
rpDescription :: Lens' RulesPackage (Maybe Text)
rpDescription = lens _rpDescription (\s a -> s {_rpDescription = a})

-- | The ARN of the rules package.
rpArn :: Lens' RulesPackage Text
rpArn = lens _rpArn (\s a -> s {_rpArn = a})

-- | The name of the rules package.
rpName :: Lens' RulesPackage Text
rpName = lens _rpName (\s a -> s {_rpName = a})

-- | The version ID of the rules package.
rpVersion :: Lens' RulesPackage Text
rpVersion = lens _rpVersion (\s a -> s {_rpVersion = a})

-- | The provider of the rules package.
rpProvider :: Lens' RulesPackage Text
rpProvider = lens _rpProvider (\s a -> s {_rpProvider = a})

instance FromJSON RulesPackage where
  parseJSON =
    withObject
      "RulesPackage"
      ( \x ->
          RulesPackage'
            <$> (x .:? "description")
            <*> (x .: "arn")
            <*> (x .: "name")
            <*> (x .: "version")
            <*> (x .: "provider")
      )

instance Hashable RulesPackage

instance NFData RulesPackage
