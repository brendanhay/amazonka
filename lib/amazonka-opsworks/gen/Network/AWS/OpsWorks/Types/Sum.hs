{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.Sum where

import Network.AWS.Prelude

data AppAttributesKeys
  = AWSFlowRubySettings
  | AutoBundleOnDeploy
  | DocumentRoot
  | RailsEnv
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AppAttributesKeys where
    parser = takeLowerText >>= \case
        "awsflowrubysettings" -> pure AWSFlowRubySettings
        "autobundleondeploy" -> pure AutoBundleOnDeploy
        "documentroot" -> pure DocumentRoot
        "railsenv" -> pure RailsEnv
        e -> fromTextError $ "Failure parsing AppAttributesKeys from value: '" <> e
           <> "'. Accepted values: awsflowrubysettings, autobundleondeploy, documentroot, railsenv"

instance ToText AppAttributesKeys where
    toText = \case
        AWSFlowRubySettings -> "AwsFlowRubySettings"
        AutoBundleOnDeploy -> "AutoBundleOnDeploy"
        DocumentRoot -> "DocumentRoot"
        RailsEnv -> "RailsEnv"

instance Hashable     AppAttributesKeys
instance NFData       AppAttributesKeys
instance ToByteString AppAttributesKeys
instance ToQuery      AppAttributesKeys
instance ToHeader     AppAttributesKeys

instance ToJSON AppAttributesKeys where
    toJSON = toJSONText

instance FromJSON AppAttributesKeys where
    parseJSON = parseJSONText "AppAttributesKeys"

data AppType
  = ATAWSFlowRuby
  | ATJava
  | ATNodejs
  | ATOther
  | ATPHP
  | ATRails
  | ATStatic
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AppType where
    parser = takeLowerText >>= \case
        "aws-flow-ruby" -> pure ATAWSFlowRuby
        "java" -> pure ATJava
        "nodejs" -> pure ATNodejs
        "other" -> pure ATOther
        "php" -> pure ATPHP
        "rails" -> pure ATRails
        "static" -> pure ATStatic
        e -> fromTextError $ "Failure parsing AppType from value: '" <> e
           <> "'. Accepted values: aws-flow-ruby, java, nodejs, other, php, rails, static"

instance ToText AppType where
    toText = \case
        ATAWSFlowRuby -> "aws-flow-ruby"
        ATJava -> "java"
        ATNodejs -> "nodejs"
        ATOther -> "other"
        ATPHP -> "php"
        ATRails -> "rails"
        ATStatic -> "static"

instance Hashable     AppType
instance NFData       AppType
instance ToByteString AppType
instance ToQuery      AppType
instance ToHeader     AppType

instance ToJSON AppType where
    toJSON = toJSONText

instance FromJSON AppType where
    parseJSON = parseJSONText "AppType"

data Architecture
  = I386
  | X86_64
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Architecture where
    parser = takeLowerText >>= \case
        "i386" -> pure I386
        "x86_64" -> pure X86_64
        e -> fromTextError $ "Failure parsing Architecture from value: '" <> e
           <> "'. Accepted values: i386, x86_64"

instance ToText Architecture where
    toText = \case
        I386 -> "i386"
        X86_64 -> "x86_64"

instance Hashable     Architecture
instance NFData       Architecture
instance ToByteString Architecture
instance ToQuery      Architecture
instance ToHeader     Architecture

instance ToJSON Architecture where
    toJSON = toJSONText

instance FromJSON Architecture where
    parseJSON = parseJSONText "Architecture"

data AutoScalingType
  = Load
  | Timer
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AutoScalingType where
    parser = takeLowerText >>= \case
        "load" -> pure Load
        "timer" -> pure Timer
        e -> fromTextError $ "Failure parsing AutoScalingType from value: '" <> e
           <> "'. Accepted values: load, timer"

instance ToText AutoScalingType where
    toText = \case
        Load -> "load"
        Timer -> "timer"

instance Hashable     AutoScalingType
instance NFData       AutoScalingType
instance ToByteString AutoScalingType
instance ToQuery      AutoScalingType
instance ToHeader     AutoScalingType

instance ToJSON AutoScalingType where
    toJSON = toJSONText

instance FromJSON AutoScalingType where
    parseJSON = parseJSONText "AutoScalingType"

-- | Specifies the encoding of the log file so that the file can be read correctly. The default is @utf_8@ . Encodings supported by Python @codecs.decode()@ can be used here.
--
--
data CloudWatchLogsEncoding
  = Ascii
  | BIG5
  | Big5hkscs
  | CP037
  | CP1006
  | CP1026
  | CP1140
  | CP1250
  | CP1251
  | CP1252
  | CP1253
  | CP1254
  | CP1255
  | CP1256
  | CP1257
  | CP1258
  | CP424
  | CP437
  | CP500
  | CP720
  | CP737
  | CP775
  | CP850
  | CP852
  | CP855
  | CP856
  | CP857
  | CP858
  | CP860
  | CP861
  | CP862
  | CP863
  | CP864
  | CP865
  | CP866
  | CP869
  | CP874
  | CP875
  | CP932
  | CP949
  | CP950
  | EucJISX0213
  | EucJis2004
  | EucJp
  | EucKr
  | GB18030
  | GB2312
  | Gbk
  | HZ
  | ISO2022Jp
  | ISO2022Jp1
  | ISO2022Jp2
  | ISO2022Jp2004
  | ISO2022Jp3
  | ISO2022JpExt
  | ISO2022Kr
  | ISO885910
  | ISO885913
  | ISO885914
  | ISO885915
  | ISO885916
  | ISO88592
  | ISO88593
  | ISO88594
  | ISO88595
  | ISO88596
  | ISO88597
  | ISO88598
  | ISO88599
  | Johab
  | KOI8r
  | KOI8u
  | Latin1
  | MACCyrillic
  | MACGreek
  | MACIceland
  | MACLATIN2
  | MACRoman
  | MACTurkish
  | PTCP154
  | ShiftJISX0213
  | ShiftJis
  | ShiftJis2004
  | Utf16
  | Utf16Be
  | Utf16Le
  | Utf32
  | Utf32Be
  | Utf32Le
  | Utf7
  | Utf8
  | Utf8Sig
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CloudWatchLogsEncoding where
    parser = takeLowerText >>= \case
        "ascii" -> pure Ascii
        "big5" -> pure BIG5
        "big5hkscs" -> pure Big5hkscs
        "cp037" -> pure CP037
        "cp1006" -> pure CP1006
        "cp1026" -> pure CP1026
        "cp1140" -> pure CP1140
        "cp1250" -> pure CP1250
        "cp1251" -> pure CP1251
        "cp1252" -> pure CP1252
        "cp1253" -> pure CP1253
        "cp1254" -> pure CP1254
        "cp1255" -> pure CP1255
        "cp1256" -> pure CP1256
        "cp1257" -> pure CP1257
        "cp1258" -> pure CP1258
        "cp424" -> pure CP424
        "cp437" -> pure CP437
        "cp500" -> pure CP500
        "cp720" -> pure CP720
        "cp737" -> pure CP737
        "cp775" -> pure CP775
        "cp850" -> pure CP850
        "cp852" -> pure CP852
        "cp855" -> pure CP855
        "cp856" -> pure CP856
        "cp857" -> pure CP857
        "cp858" -> pure CP858
        "cp860" -> pure CP860
        "cp861" -> pure CP861
        "cp862" -> pure CP862
        "cp863" -> pure CP863
        "cp864" -> pure CP864
        "cp865" -> pure CP865
        "cp866" -> pure CP866
        "cp869" -> pure CP869
        "cp874" -> pure CP874
        "cp875" -> pure CP875
        "cp932" -> pure CP932
        "cp949" -> pure CP949
        "cp950" -> pure CP950
        "euc_jisx0213" -> pure EucJISX0213
        "euc_jis_2004" -> pure EucJis2004
        "euc_jp" -> pure EucJp
        "euc_kr" -> pure EucKr
        "gb18030" -> pure GB18030
        "gb2312" -> pure GB2312
        "gbk" -> pure Gbk
        "hz" -> pure HZ
        "iso2022_jp" -> pure ISO2022Jp
        "iso2022_jp_1" -> pure ISO2022Jp1
        "iso2022_jp_2" -> pure ISO2022Jp2
        "iso2022_jp_2004" -> pure ISO2022Jp2004
        "iso2022_jp_3" -> pure ISO2022Jp3
        "iso2022_jp_ext" -> pure ISO2022JpExt
        "iso2022_kr" -> pure ISO2022Kr
        "iso8859_10" -> pure ISO885910
        "iso8859_13" -> pure ISO885913
        "iso8859_14" -> pure ISO885914
        "iso8859_15" -> pure ISO885915
        "iso8859_16" -> pure ISO885916
        "iso8859_2" -> pure ISO88592
        "iso8859_3" -> pure ISO88593
        "iso8859_4" -> pure ISO88594
        "iso8859_5" -> pure ISO88595
        "iso8859_6" -> pure ISO88596
        "iso8859_7" -> pure ISO88597
        "iso8859_8" -> pure ISO88598
        "iso8859_9" -> pure ISO88599
        "johab" -> pure Johab
        "koi8_r" -> pure KOI8r
        "koi8_u" -> pure KOI8u
        "latin_1" -> pure Latin1
        "mac_cyrillic" -> pure MACCyrillic
        "mac_greek" -> pure MACGreek
        "mac_iceland" -> pure MACIceland
        "mac_latin2" -> pure MACLATIN2
        "mac_roman" -> pure MACRoman
        "mac_turkish" -> pure MACTurkish
        "ptcp154" -> pure PTCP154
        "shift_jisx0213" -> pure ShiftJISX0213
        "shift_jis" -> pure ShiftJis
        "shift_jis_2004" -> pure ShiftJis2004
        "utf_16" -> pure Utf16
        "utf_16_be" -> pure Utf16Be
        "utf_16_le" -> pure Utf16Le
        "utf_32" -> pure Utf32
        "utf_32_be" -> pure Utf32Be
        "utf_32_le" -> pure Utf32Le
        "utf_7" -> pure Utf7
        "utf_8" -> pure Utf8
        "utf_8_sig" -> pure Utf8Sig
        e -> fromTextError $ "Failure parsing CloudWatchLogsEncoding from value: '" <> e
           <> "'. Accepted values: ascii, big5, big5hkscs, cp037, cp1006, cp1026, cp1140, cp1250, cp1251, cp1252, cp1253, cp1254, cp1255, cp1256, cp1257, cp1258, cp424, cp437, cp500, cp720, cp737, cp775, cp850, cp852, cp855, cp856, cp857, cp858, cp860, cp861, cp862, cp863, cp864, cp865, cp866, cp869, cp874, cp875, cp932, cp949, cp950, euc_jisx0213, euc_jis_2004, euc_jp, euc_kr, gb18030, gb2312, gbk, hz, iso2022_jp, iso2022_jp_1, iso2022_jp_2, iso2022_jp_2004, iso2022_jp_3, iso2022_jp_ext, iso2022_kr, iso8859_10, iso8859_13, iso8859_14, iso8859_15, iso8859_16, iso8859_2, iso8859_3, iso8859_4, iso8859_5, iso8859_6, iso8859_7, iso8859_8, iso8859_9, johab, koi8_r, koi8_u, latin_1, mac_cyrillic, mac_greek, mac_iceland, mac_latin2, mac_roman, mac_turkish, ptcp154, shift_jisx0213, shift_jis, shift_jis_2004, utf_16, utf_16_be, utf_16_le, utf_32, utf_32_be, utf_32_le, utf_7, utf_8, utf_8_sig"

instance ToText CloudWatchLogsEncoding where
    toText = \case
        Ascii -> "ascii"
        BIG5 -> "big5"
        Big5hkscs -> "big5hkscs"
        CP037 -> "cp037"
        CP1006 -> "cp1006"
        CP1026 -> "cp1026"
        CP1140 -> "cp1140"
        CP1250 -> "cp1250"
        CP1251 -> "cp1251"
        CP1252 -> "cp1252"
        CP1253 -> "cp1253"
        CP1254 -> "cp1254"
        CP1255 -> "cp1255"
        CP1256 -> "cp1256"
        CP1257 -> "cp1257"
        CP1258 -> "cp1258"
        CP424 -> "cp424"
        CP437 -> "cp437"
        CP500 -> "cp500"
        CP720 -> "cp720"
        CP737 -> "cp737"
        CP775 -> "cp775"
        CP850 -> "cp850"
        CP852 -> "cp852"
        CP855 -> "cp855"
        CP856 -> "cp856"
        CP857 -> "cp857"
        CP858 -> "cp858"
        CP860 -> "cp860"
        CP861 -> "cp861"
        CP862 -> "cp862"
        CP863 -> "cp863"
        CP864 -> "cp864"
        CP865 -> "cp865"
        CP866 -> "cp866"
        CP869 -> "cp869"
        CP874 -> "cp874"
        CP875 -> "cp875"
        CP932 -> "cp932"
        CP949 -> "cp949"
        CP950 -> "cp950"
        EucJISX0213 -> "euc_jisx0213"
        EucJis2004 -> "euc_jis_2004"
        EucJp -> "euc_jp"
        EucKr -> "euc_kr"
        GB18030 -> "gb18030"
        GB2312 -> "gb2312"
        Gbk -> "gbk"
        HZ -> "hz"
        ISO2022Jp -> "iso2022_jp"
        ISO2022Jp1 -> "iso2022_jp_1"
        ISO2022Jp2 -> "iso2022_jp_2"
        ISO2022Jp2004 -> "iso2022_jp_2004"
        ISO2022Jp3 -> "iso2022_jp_3"
        ISO2022JpExt -> "iso2022_jp_ext"
        ISO2022Kr -> "iso2022_kr"
        ISO885910 -> "iso8859_10"
        ISO885913 -> "iso8859_13"
        ISO885914 -> "iso8859_14"
        ISO885915 -> "iso8859_15"
        ISO885916 -> "iso8859_16"
        ISO88592 -> "iso8859_2"
        ISO88593 -> "iso8859_3"
        ISO88594 -> "iso8859_4"
        ISO88595 -> "iso8859_5"
        ISO88596 -> "iso8859_6"
        ISO88597 -> "iso8859_7"
        ISO88598 -> "iso8859_8"
        ISO88599 -> "iso8859_9"
        Johab -> "johab"
        KOI8r -> "koi8_r"
        KOI8u -> "koi8_u"
        Latin1 -> "latin_1"
        MACCyrillic -> "mac_cyrillic"
        MACGreek -> "mac_greek"
        MACIceland -> "mac_iceland"
        MACLATIN2 -> "mac_latin2"
        MACRoman -> "mac_roman"
        MACTurkish -> "mac_turkish"
        PTCP154 -> "ptcp154"
        ShiftJISX0213 -> "shift_jisx0213"
        ShiftJis -> "shift_jis"
        ShiftJis2004 -> "shift_jis_2004"
        Utf16 -> "utf_16"
        Utf16Be -> "utf_16_be"
        Utf16Le -> "utf_16_le"
        Utf32 -> "utf_32"
        Utf32Be -> "utf_32_be"
        Utf32Le -> "utf_32_le"
        Utf7 -> "utf_7"
        Utf8 -> "utf_8"
        Utf8Sig -> "utf_8_sig"

instance Hashable     CloudWatchLogsEncoding
instance NFData       CloudWatchLogsEncoding
instance ToByteString CloudWatchLogsEncoding
instance ToQuery      CloudWatchLogsEncoding
instance ToHeader     CloudWatchLogsEncoding

instance ToJSON CloudWatchLogsEncoding where
    toJSON = toJSONText

instance FromJSON CloudWatchLogsEncoding where
    parseJSON = parseJSONText "CloudWatchLogsEncoding"

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.
--
--
data CloudWatchLogsInitialPosition
  = EndOfFile
  | StartOfFile
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CloudWatchLogsInitialPosition where
    parser = takeLowerText >>= \case
        "end_of_file" -> pure EndOfFile
        "start_of_file" -> pure StartOfFile
        e -> fromTextError $ "Failure parsing CloudWatchLogsInitialPosition from value: '" <> e
           <> "'. Accepted values: end_of_file, start_of_file"

instance ToText CloudWatchLogsInitialPosition where
    toText = \case
        EndOfFile -> "end_of_file"
        StartOfFile -> "start_of_file"

instance Hashable     CloudWatchLogsInitialPosition
instance NFData       CloudWatchLogsInitialPosition
instance ToByteString CloudWatchLogsInitialPosition
instance ToQuery      CloudWatchLogsInitialPosition
instance ToHeader     CloudWatchLogsInitialPosition

instance ToJSON CloudWatchLogsInitialPosition where
    toJSON = toJSONText

instance FromJSON CloudWatchLogsInitialPosition where
    parseJSON = parseJSONText "CloudWatchLogsInitialPosition"

-- | The preferred time zone for logs streamed to CloudWatch Logs. Valid values are @LOCAL@ and @UTC@ , for Coordinated Universal Time.
--
--
data CloudWatchLogsTimeZone
  = Local
  | Utc
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText CloudWatchLogsTimeZone where
    parser = takeLowerText >>= \case
        "local" -> pure Local
        "utc" -> pure Utc
        e -> fromTextError $ "Failure parsing CloudWatchLogsTimeZone from value: '" <> e
           <> "'. Accepted values: local, utc"

instance ToText CloudWatchLogsTimeZone where
    toText = \case
        Local -> "LOCAL"
        Utc -> "UTC"

instance Hashable     CloudWatchLogsTimeZone
instance NFData       CloudWatchLogsTimeZone
instance ToByteString CloudWatchLogsTimeZone
instance ToQuery      CloudWatchLogsTimeZone
instance ToHeader     CloudWatchLogsTimeZone

instance ToJSON CloudWatchLogsTimeZone where
    toJSON = toJSONText

instance FromJSON CloudWatchLogsTimeZone where
    parseJSON = parseJSONText "CloudWatchLogsTimeZone"

data DeploymentCommandName
  = Configure
  | Deploy
  | ExecuteRecipes
  | InstallDependencies
  | Restart
  | Rollback
  | Setup
  | Start
  | Stop
  | Undeploy
  | UpdateCustomCookbooks
  | UpdateDependencies
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DeploymentCommandName where
    parser = takeLowerText >>= \case
        "configure" -> pure Configure
        "deploy" -> pure Deploy
        "execute_recipes" -> pure ExecuteRecipes
        "install_dependencies" -> pure InstallDependencies
        "restart" -> pure Restart
        "rollback" -> pure Rollback
        "setup" -> pure Setup
        "start" -> pure Start
        "stop" -> pure Stop
        "undeploy" -> pure Undeploy
        "update_custom_cookbooks" -> pure UpdateCustomCookbooks
        "update_dependencies" -> pure UpdateDependencies
        e -> fromTextError $ "Failure parsing DeploymentCommandName from value: '" <> e
           <> "'. Accepted values: configure, deploy, execute_recipes, install_dependencies, restart, rollback, setup, start, stop, undeploy, update_custom_cookbooks, update_dependencies"

instance ToText DeploymentCommandName where
    toText = \case
        Configure -> "configure"
        Deploy -> "deploy"
        ExecuteRecipes -> "execute_recipes"
        InstallDependencies -> "install_dependencies"
        Restart -> "restart"
        Rollback -> "rollback"
        Setup -> "setup"
        Start -> "start"
        Stop -> "stop"
        Undeploy -> "undeploy"
        UpdateCustomCookbooks -> "update_custom_cookbooks"
        UpdateDependencies -> "update_dependencies"

instance Hashable     DeploymentCommandName
instance NFData       DeploymentCommandName
instance ToByteString DeploymentCommandName
instance ToQuery      DeploymentCommandName
instance ToHeader     DeploymentCommandName

instance ToJSON DeploymentCommandName where
    toJSON = toJSONText

instance FromJSON DeploymentCommandName where
    parseJSON = parseJSONText "DeploymentCommandName"

data LayerAttributesKeys
  = BundlerVersion
  | EcsClusterARN
  | EnableHaproxyStats
  | GangliaPassword
  | GangliaURL
  | GangliaUser
  | HaproxyHealthCheckMethod
  | HaproxyHealthCheckURL
  | HaproxyStatsPassword
  | HaproxyStatsURL
  | HaproxyStatsUser
  | JVM
  | JVMOptions
  | JVMVersion
  | JavaAppServer
  | JavaAppServerVersion
  | ManageBundler
  | MemcachedMemory
  | MysqlRootPassword
  | MysqlRootPasswordUbiquitous
  | NodejsVersion
  | PassengerVersion
  | RailsStack
  | RubyVersion
  | RubygemsVersion
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LayerAttributesKeys where
    parser = takeLowerText >>= \case
        "bundlerversion" -> pure BundlerVersion
        "ecsclusterarn" -> pure EcsClusterARN
        "enablehaproxystats" -> pure EnableHaproxyStats
        "gangliapassword" -> pure GangliaPassword
        "gangliaurl" -> pure GangliaURL
        "gangliauser" -> pure GangliaUser
        "haproxyhealthcheckmethod" -> pure HaproxyHealthCheckMethod
        "haproxyhealthcheckurl" -> pure HaproxyHealthCheckURL
        "haproxystatspassword" -> pure HaproxyStatsPassword
        "haproxystatsurl" -> pure HaproxyStatsURL
        "haproxystatsuser" -> pure HaproxyStatsUser
        "jvm" -> pure JVM
        "jvmoptions" -> pure JVMOptions
        "jvmversion" -> pure JVMVersion
        "javaappserver" -> pure JavaAppServer
        "javaappserverversion" -> pure JavaAppServerVersion
        "managebundler" -> pure ManageBundler
        "memcachedmemory" -> pure MemcachedMemory
        "mysqlrootpassword" -> pure MysqlRootPassword
        "mysqlrootpasswordubiquitous" -> pure MysqlRootPasswordUbiquitous
        "nodejsversion" -> pure NodejsVersion
        "passengerversion" -> pure PassengerVersion
        "railsstack" -> pure RailsStack
        "rubyversion" -> pure RubyVersion
        "rubygemsversion" -> pure RubygemsVersion
        e -> fromTextError $ "Failure parsing LayerAttributesKeys from value: '" <> e
           <> "'. Accepted values: bundlerversion, ecsclusterarn, enablehaproxystats, gangliapassword, gangliaurl, gangliauser, haproxyhealthcheckmethod, haproxyhealthcheckurl, haproxystatspassword, haproxystatsurl, haproxystatsuser, jvm, jvmoptions, jvmversion, javaappserver, javaappserverversion, managebundler, memcachedmemory, mysqlrootpassword, mysqlrootpasswordubiquitous, nodejsversion, passengerversion, railsstack, rubyversion, rubygemsversion"

instance ToText LayerAttributesKeys where
    toText = \case
        BundlerVersion -> "BundlerVersion"
        EcsClusterARN -> "EcsClusterArn"
        EnableHaproxyStats -> "EnableHaproxyStats"
        GangliaPassword -> "GangliaPassword"
        GangliaURL -> "GangliaUrl"
        GangliaUser -> "GangliaUser"
        HaproxyHealthCheckMethod -> "HaproxyHealthCheckMethod"
        HaproxyHealthCheckURL -> "HaproxyHealthCheckUrl"
        HaproxyStatsPassword -> "HaproxyStatsPassword"
        HaproxyStatsURL -> "HaproxyStatsUrl"
        HaproxyStatsUser -> "HaproxyStatsUser"
        JVM -> "Jvm"
        JVMOptions -> "JvmOptions"
        JVMVersion -> "JvmVersion"
        JavaAppServer -> "JavaAppServer"
        JavaAppServerVersion -> "JavaAppServerVersion"
        ManageBundler -> "ManageBundler"
        MemcachedMemory -> "MemcachedMemory"
        MysqlRootPassword -> "MysqlRootPassword"
        MysqlRootPasswordUbiquitous -> "MysqlRootPasswordUbiquitous"
        NodejsVersion -> "NodejsVersion"
        PassengerVersion -> "PassengerVersion"
        RailsStack -> "RailsStack"
        RubyVersion -> "RubyVersion"
        RubygemsVersion -> "RubygemsVersion"

instance Hashable     LayerAttributesKeys
instance NFData       LayerAttributesKeys
instance ToByteString LayerAttributesKeys
instance ToQuery      LayerAttributesKeys
instance ToHeader     LayerAttributesKeys

instance ToJSON LayerAttributesKeys where
    toJSON = toJSONText

instance FromJSON LayerAttributesKeys where
    parseJSON = parseJSONText "LayerAttributesKeys"

data LayerType
  = AWSFlowRuby
  | Custom
  | DBMaster
  | EcsCluster
  | JavaApp
  | LB
  | Memcached
  | MonitoringMaster
  | NodejsApp
  | PHPApp
  | RailsApp
  | Web
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText LayerType where
    parser = takeLowerText >>= \case
        "aws-flow-ruby" -> pure AWSFlowRuby
        "custom" -> pure Custom
        "db-master" -> pure DBMaster
        "ecs-cluster" -> pure EcsCluster
        "java-app" -> pure JavaApp
        "lb" -> pure LB
        "memcached" -> pure Memcached
        "monitoring-master" -> pure MonitoringMaster
        "nodejs-app" -> pure NodejsApp
        "php-app" -> pure PHPApp
        "rails-app" -> pure RailsApp
        "web" -> pure Web
        e -> fromTextError $ "Failure parsing LayerType from value: '" <> e
           <> "'. Accepted values: aws-flow-ruby, custom, db-master, ecs-cluster, java-app, lb, memcached, monitoring-master, nodejs-app, php-app, rails-app, web"

instance ToText LayerType where
    toText = \case
        AWSFlowRuby -> "aws-flow-ruby"
        Custom -> "custom"
        DBMaster -> "db-master"
        EcsCluster -> "ecs-cluster"
        JavaApp -> "java-app"
        LB -> "lb"
        Memcached -> "memcached"
        MonitoringMaster -> "monitoring-master"
        NodejsApp -> "nodejs-app"
        PHPApp -> "php-app"
        RailsApp -> "rails-app"
        Web -> "web"

instance Hashable     LayerType
instance NFData       LayerType
instance ToByteString LayerType
instance ToQuery      LayerType
instance ToHeader     LayerType

instance ToJSON LayerType where
    toJSON = toJSONText

instance FromJSON LayerType where
    parseJSON = parseJSONText "LayerType"

data RootDeviceType
  = EBS
  | InstanceStore
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RootDeviceType where
    parser = takeLowerText >>= \case
        "ebs" -> pure EBS
        "instance-store" -> pure InstanceStore
        e -> fromTextError $ "Failure parsing RootDeviceType from value: '" <> e
           <> "'. Accepted values: ebs, instance-store"

instance ToText RootDeviceType where
    toText = \case
        EBS -> "ebs"
        InstanceStore -> "instance-store"

instance Hashable     RootDeviceType
instance NFData       RootDeviceType
instance ToByteString RootDeviceType
instance ToQuery      RootDeviceType
instance ToHeader     RootDeviceType

instance ToJSON RootDeviceType where
    toJSON = toJSONText

instance FromJSON RootDeviceType where
    parseJSON = parseJSONText "RootDeviceType"

data SourceType
  = Archive
  | Git
  | S3
  | SVN
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText SourceType where
    parser = takeLowerText >>= \case
        "archive" -> pure Archive
        "git" -> pure Git
        "s3" -> pure S3
        "svn" -> pure SVN
        e -> fromTextError $ "Failure parsing SourceType from value: '" <> e
           <> "'. Accepted values: archive, git, s3, svn"

instance ToText SourceType where
    toText = \case
        Archive -> "archive"
        Git -> "git"
        S3 -> "s3"
        SVN -> "svn"

instance Hashable     SourceType
instance NFData       SourceType
instance ToByteString SourceType
instance ToQuery      SourceType
instance ToHeader     SourceType

instance ToJSON SourceType where
    toJSON = toJSONText

instance FromJSON SourceType where
    parseJSON = parseJSONText "SourceType"

data StackAttributesKeys =
  Color
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StackAttributesKeys where
    parser = takeLowerText >>= \case
        "color" -> pure Color
        e -> fromTextError $ "Failure parsing StackAttributesKeys from value: '" <> e
           <> "'. Accepted values: color"

instance ToText StackAttributesKeys where
    toText = \case
        Color -> "Color"

instance Hashable     StackAttributesKeys
instance NFData       StackAttributesKeys
instance ToByteString StackAttributesKeys
instance ToQuery      StackAttributesKeys
instance ToHeader     StackAttributesKeys

instance ToJSON StackAttributesKeys where
    toJSON = toJSONText

instance FromJSON StackAttributesKeys where
    parseJSON = parseJSONText "StackAttributesKeys"

data VirtualizationType
  = HVM
  | Paravirtual
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VirtualizationType where
    parser = takeLowerText >>= \case
        "hvm" -> pure HVM
        "paravirtual" -> pure Paravirtual
        e -> fromTextError $ "Failure parsing VirtualizationType from value: '" <> e
           <> "'. Accepted values: hvm, paravirtual"

instance ToText VirtualizationType where
    toText = \case
        HVM -> "hvm"
        Paravirtual -> "paravirtual"

instance Hashable     VirtualizationType
instance NFData       VirtualizationType
instance ToByteString VirtualizationType
instance ToQuery      VirtualizationType
instance ToHeader     VirtualizationType

instance FromJSON VirtualizationType where
    parseJSON = parseJSONText "VirtualizationType"

data VolumeType
  = GP2
  | IO1
  | Standard
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText VolumeType where
    parser = takeLowerText >>= \case
        "gp2" -> pure GP2
        "io1" -> pure IO1
        "standard" -> pure Standard
        e -> fromTextError $ "Failure parsing VolumeType from value: '" <> e
           <> "'. Accepted values: gp2, io1, standard"

instance ToText VolumeType where
    toText = \case
        GP2 -> "gp2"
        IO1 -> "io1"
        Standard -> "standard"

instance Hashable     VolumeType
instance NFData       VolumeType
instance ToByteString VolumeType
instance ToQuery      VolumeType
instance ToHeader     VolumeType

instance ToJSON VolumeType where
    toJSON = toJSONText

instance FromJSON VolumeType where
    parseJSON = parseJSONText "VolumeType"
