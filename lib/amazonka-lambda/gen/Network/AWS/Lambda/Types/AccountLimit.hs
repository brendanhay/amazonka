{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AccountLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AccountLimit where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Limits that are related to concurrency and storage. All file and storage sizes are in bytes.
--
--
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { _alConcurrentExecutions ::
      !(Maybe Int),
    _alTotalCodeSize :: !(Maybe Integer),
    _alUnreservedConcurrentExecutions :: !(Maybe Nat),
    _alCodeSizeUnzipped :: !(Maybe Integer),
    _alCodeSizeZipped :: !(Maybe Integer)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alConcurrentExecutions' - The maximum number of simultaneous function executions.
--
-- * 'alTotalCodeSize' - The amount of storage space that you can use for all deployment packages and layer archives.
--
-- * 'alUnreservedConcurrentExecutions' - The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
--
-- * 'alCodeSizeUnzipped' - The maximum size of a function's deployment package and layers when they're extracted.
--
-- * 'alCodeSizeZipped' - The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
accountLimit ::
  AccountLimit
accountLimit =
  AccountLimit'
    { _alConcurrentExecutions = Nothing,
      _alTotalCodeSize = Nothing,
      _alUnreservedConcurrentExecutions = Nothing,
      _alCodeSizeUnzipped = Nothing,
      _alCodeSizeZipped = Nothing
    }

-- | The maximum number of simultaneous function executions.
alConcurrentExecutions :: Lens' AccountLimit (Maybe Int)
alConcurrentExecutions = lens _alConcurrentExecutions (\s a -> s {_alConcurrentExecutions = a})

-- | The amount of storage space that you can use for all deployment packages and layer archives.
alTotalCodeSize :: Lens' AccountLimit (Maybe Integer)
alTotalCodeSize = lens _alTotalCodeSize (\s a -> s {_alTotalCodeSize = a})

-- | The maximum number of simultaneous function executions, minus the capacity that's reserved for individual functions with 'PutFunctionConcurrency' .
alUnreservedConcurrentExecutions :: Lens' AccountLimit (Maybe Natural)
alUnreservedConcurrentExecutions = lens _alUnreservedConcurrentExecutions (\s a -> s {_alUnreservedConcurrentExecutions = a}) . mapping _Nat

-- | The maximum size of a function's deployment package and layers when they're extracted.
alCodeSizeUnzipped :: Lens' AccountLimit (Maybe Integer)
alCodeSizeUnzipped = lens _alCodeSizeUnzipped (\s a -> s {_alCodeSizeUnzipped = a})

-- | The maximum size of a deployment package when it's uploaded directly to AWS Lambda. Use Amazon S3 for larger files.
alCodeSizeZipped :: Lens' AccountLimit (Maybe Integer)
alCodeSizeZipped = lens _alCodeSizeZipped (\s a -> s {_alCodeSizeZipped = a})

instance FromJSON AccountLimit where
  parseJSON =
    withObject
      "AccountLimit"
      ( \x ->
          AccountLimit'
            <$> (x .:? "ConcurrentExecutions")
            <*> (x .:? "TotalCodeSize")
            <*> (x .:? "UnreservedConcurrentExecutions")
            <*> (x .:? "CodeSizeUnzipped")
            <*> (x .:? "CodeSizeZipped")
      )

instance Hashable AccountLimit

instance NFData AccountLimit
