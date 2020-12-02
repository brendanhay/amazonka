{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSInfo where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.EBSEncryptionSupport
import Network.AWS.EC2.Types.EBSNvmeSupport
import Network.AWS.EC2.Types.EBSOptimizedInfo
import Network.AWS.EC2.Types.EBSOptimizedSupport
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the Amazon EBS features supported by the instance type.
--
--
--
-- /See:/ 'ebsInfo' smart constructor.
data EBSInfo = EBSInfo'
  { _eiEBSOptimizedInfo ::
      !(Maybe EBSOptimizedInfo),
    _eiEncryptionSupport :: !(Maybe EBSEncryptionSupport),
    _eiEBSOptimizedSupport :: !(Maybe EBSOptimizedSupport),
    _eiNvmeSupport :: !(Maybe EBSNvmeSupport)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EBSInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiEBSOptimizedInfo' - Describes the optimized EBS performance for the instance type.
--
-- * 'eiEncryptionSupport' - Indicates whether Amazon EBS encryption is supported.
--
-- * 'eiEBSOptimizedSupport' - Indicates whether the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
--
-- * 'eiNvmeSupport' - Indicates whether non-volatile memory express (NVMe) is supported.
ebsInfo ::
  EBSInfo
ebsInfo =
  EBSInfo'
    { _eiEBSOptimizedInfo = Nothing,
      _eiEncryptionSupport = Nothing,
      _eiEBSOptimizedSupport = Nothing,
      _eiNvmeSupport = Nothing
    }

-- | Describes the optimized EBS performance for the instance type.
eiEBSOptimizedInfo :: Lens' EBSInfo (Maybe EBSOptimizedInfo)
eiEBSOptimizedInfo = lens _eiEBSOptimizedInfo (\s a -> s {_eiEBSOptimizedInfo = a})

-- | Indicates whether Amazon EBS encryption is supported.
eiEncryptionSupport :: Lens' EBSInfo (Maybe EBSEncryptionSupport)
eiEncryptionSupport = lens _eiEncryptionSupport (\s a -> s {_eiEncryptionSupport = a})

-- | Indicates whether the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
eiEBSOptimizedSupport :: Lens' EBSInfo (Maybe EBSOptimizedSupport)
eiEBSOptimizedSupport = lens _eiEBSOptimizedSupport (\s a -> s {_eiEBSOptimizedSupport = a})

-- | Indicates whether non-volatile memory express (NVMe) is supported.
eiNvmeSupport :: Lens' EBSInfo (Maybe EBSNvmeSupport)
eiNvmeSupport = lens _eiNvmeSupport (\s a -> s {_eiNvmeSupport = a})

instance FromXML EBSInfo where
  parseXML x =
    EBSInfo'
      <$> (x .@? "ebsOptimizedInfo")
      <*> (x .@? "encryptionSupport")
      <*> (x .@? "ebsOptimizedSupport")
      <*> (x .@? "nvmeSupport")

instance Hashable EBSInfo

instance NFData EBSInfo
