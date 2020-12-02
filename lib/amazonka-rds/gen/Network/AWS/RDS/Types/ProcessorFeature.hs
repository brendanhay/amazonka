{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ProcessorFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ProcessorFeature where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the processor features of a DB instance class.
--
--
-- To specify the number of CPU cores, use the @coreCount@ feature name for the @Name@ parameter. To specify the number of threads per core, use the @threadsPerCore@ feature name for the @Name@ parameter.
--
-- You can set the processor features of the DB instance class for a DB instance when you call one of the following actions:
--
--     * @CreateDBInstance@
--
--     * @ModifyDBInstance@
--
--     * @RestoreDBInstanceFromDBSnapshot@
--
--     * @RestoreDBInstanceFromS3@
--
--     * @RestoreDBInstanceToPointInTime@
--
--
--
-- You can view the valid processor values for a particular instance class by calling the @DescribeOrderableDBInstanceOptions@ action and specifying the instance class for the @DBInstanceClass@ parameter.
--
-- In addition, you can use the following actions for DB instance class processor information:
--
--     * @DescribeDBInstances@
--
--     * @DescribeDBSnapshots@
--
--     * @DescribeValidDBInstanceModifications@
--
--
--
-- If you call @DescribeDBInstances@ , @ProcessorFeature@ returns non-null values only if the following conditions are met:
--
--     * You are accessing an Oracle DB instance.
--
--     * Your Oracle DB instance class supports configuring the number of CPU cores and threads per core.
--
--     * The current number CPU cores and threads is set to a non-default value.
--
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class> in the /Amazon RDS User Guide. /
--
--
-- /See:/ 'processorFeature' smart constructor.
data ProcessorFeature = ProcessorFeature'
  { _pfValue ::
      !(Maybe Text),
    _pfName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessorFeature' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfValue' - The value of a processor feature name.
--
-- * 'pfName' - The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
processorFeature ::
  ProcessorFeature
processorFeature =
  ProcessorFeature' {_pfValue = Nothing, _pfName = Nothing}

-- | The value of a processor feature name.
pfValue :: Lens' ProcessorFeature (Maybe Text)
pfValue = lens _pfValue (\s a -> s {_pfValue = a})

-- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
pfName :: Lens' ProcessorFeature (Maybe Text)
pfName = lens _pfName (\s a -> s {_pfName = a})

instance FromXML ProcessorFeature where
  parseXML x =
    ProcessorFeature' <$> (x .@? "Value") <*> (x .@? "Name")

instance Hashable ProcessorFeature

instance NFData ProcessorFeature

instance ToQuery ProcessorFeature where
  toQuery ProcessorFeature' {..} =
    mconcat ["Value" =: _pfValue, "Name" =: _pfName]
