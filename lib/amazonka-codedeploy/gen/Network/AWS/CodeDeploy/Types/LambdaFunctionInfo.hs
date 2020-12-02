{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LambdaFunctionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LambdaFunctionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a Lambda function specified in a deployment.
--
--
--
-- /See:/ 'lambdaFunctionInfo' smart constructor.
data LambdaFunctionInfo = LambdaFunctionInfo'
  { _lfiCurrentVersion ::
      !(Maybe Text),
    _lfiFunctionAlias :: !(Maybe Text),
    _lfiFunctionName :: !(Maybe Text),
    _lfiTargetVersion :: !(Maybe Text),
    _lfiTargetVersionWeight :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaFunctionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfiCurrentVersion' - The version of a Lambda function that production traffic points to.
--
-- * 'lfiFunctionAlias' - The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
--
-- * 'lfiFunctionName' - The name of a Lambda function.
--
-- * 'lfiTargetVersion' - The version of a Lambda function that production traffic points to after the Lambda function is deployed.
--
-- * 'lfiTargetVersionWeight' - The percentage of production traffic that the target version of a Lambda function receives.
lambdaFunctionInfo ::
  LambdaFunctionInfo
lambdaFunctionInfo =
  LambdaFunctionInfo'
    { _lfiCurrentVersion = Nothing,
      _lfiFunctionAlias = Nothing,
      _lfiFunctionName = Nothing,
      _lfiTargetVersion = Nothing,
      _lfiTargetVersionWeight = Nothing
    }

-- | The version of a Lambda function that production traffic points to.
lfiCurrentVersion :: Lens' LambdaFunctionInfo (Maybe Text)
lfiCurrentVersion = lens _lfiCurrentVersion (\s a -> s {_lfiCurrentVersion = a})

-- | The alias of a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html AWS Lambda Function Aliases> in the /AWS Lambda Developer Guide/ .
lfiFunctionAlias :: Lens' LambdaFunctionInfo (Maybe Text)
lfiFunctionAlias = lens _lfiFunctionAlias (\s a -> s {_lfiFunctionAlias = a})

-- | The name of a Lambda function.
lfiFunctionName :: Lens' LambdaFunctionInfo (Maybe Text)
lfiFunctionName = lens _lfiFunctionName (\s a -> s {_lfiFunctionName = a})

-- | The version of a Lambda function that production traffic points to after the Lambda function is deployed.
lfiTargetVersion :: Lens' LambdaFunctionInfo (Maybe Text)
lfiTargetVersion = lens _lfiTargetVersion (\s a -> s {_lfiTargetVersion = a})

-- | The percentage of production traffic that the target version of a Lambda function receives.
lfiTargetVersionWeight :: Lens' LambdaFunctionInfo (Maybe Double)
lfiTargetVersionWeight = lens _lfiTargetVersionWeight (\s a -> s {_lfiTargetVersionWeight = a})

instance FromJSON LambdaFunctionInfo where
  parseJSON =
    withObject
      "LambdaFunctionInfo"
      ( \x ->
          LambdaFunctionInfo'
            <$> (x .:? "currentVersion")
            <*> (x .:? "functionAlias")
            <*> (x .:? "functionName")
            <*> (x .:? "targetVersion")
            <*> (x .:? "targetVersionWeight")
      )

instance Hashable LambdaFunctionInfo

instance NFData LambdaFunctionInfo
