{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Sum

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
--
--
-- /See:/ 'accountLimit' smart constructor.
data AccountLimit = AccountLimit'
  { _alType  :: !AccountLimitType
  , _alValue :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'alType' - The limit that you requested. Valid values include the following:     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
--
-- * 'alValue' - The current value for the limit that is specified by 'AccountLimit$Type' .
accountLimit
    :: AccountLimitType -- ^ 'alType'
    -> Natural -- ^ 'alValue'
    -> AccountLimit
accountLimit pType_ pValue_ =
  AccountLimit' {_alType = pType_, _alValue = _Nat # pValue_}


-- | The limit that you requested. Valid values include the following:     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
alType :: Lens' AccountLimit AccountLimitType
alType = lens _alType (\ s a -> s{_alType = a})

-- | The current value for the limit that is specified by 'AccountLimit$Type' .
alValue :: Lens' AccountLimit Natural
alValue = lens _alValue (\ s a -> s{_alValue = a}) . _Nat

instance FromXML AccountLimit where
        parseXML x
          = AccountLimit' <$> (x .@ "Type") <*> (x .@ "Value")

instance Hashable AccountLimit where

instance NFData AccountLimit where

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
--
--
--
-- /See:/ 'alarmIdentifier' smart constructor.
data AlarmIdentifier = AlarmIdentifier'
  { _aiRegion :: !CloudWatchRegion
  , _aiName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AlarmIdentifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiRegion' - A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy. For the current list of CloudWatch regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Regions and Endpoints/ chapter of the /Amazon Web Services General Reference/ .
--
-- * 'aiName' - The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
alarmIdentifier
    :: CloudWatchRegion -- ^ 'aiRegion'
    -> Text -- ^ 'aiName'
    -> AlarmIdentifier
alarmIdentifier pRegion_ pName_ =
  AlarmIdentifier' {_aiRegion = pRegion_, _aiName = pName_}


-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy. For the current list of CloudWatch regions, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#cw_region Amazon CloudWatch> in the /AWS Regions and Endpoints/ chapter of the /Amazon Web Services General Reference/ .
aiRegion :: Lens' AlarmIdentifier CloudWatchRegion
aiRegion = lens _aiRegion (\ s a -> s{_aiRegion = a})

-- | The name of the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
aiName :: Lens' AlarmIdentifier Text
aiName = lens _aiName (\ s a -> s{_aiName = a})

instance FromXML AlarmIdentifier where
        parseXML x
          = AlarmIdentifier' <$>
              (x .@ "Region") <*> (x .@ "Name")

instance Hashable AlarmIdentifier where

instance NFData AlarmIdentifier where

instance ToXML AlarmIdentifier where
        toXML AlarmIdentifier'{..}
          = mconcat ["Region" @= _aiRegion, "Name" @= _aiName]

-- | /Alias resource record sets only:/ Information about the CloudFront distribution, Elastic Beanstalk environment, ELB load balancer, Amazon S3 bucket, or Amazon Route 53 resource record set that you're redirecting queries to. An Elastic Beanstalk environment must have a regionalized subdomain.
--
--
-- When creating resource record sets for a private hosted zone, note the following:
--
--     * Resource record sets can't be created for CloudFront distributions in a private hosted zone.
--
--     * Creating geolocation alias resource record sets or latency alias resource record sets in a private hosted zone is unsupported.
--
--     * For information about creating failover resource record sets in a private hosted zone, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone> .
--
--
--
--
-- /See:/ 'aliasTarget' smart constructor.
data AliasTarget = AliasTarget'
  { _atHostedZoneId         :: !ResourceId
  , _atDNSName              :: !Text
  , _atEvaluateTargetHealth :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AliasTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atHostedZoneId' - /Alias resource records sets only/ : The value used depends on where you want to route traffic:     * CloudFront distribution    * Specify @Z2FDTNDATAQYW2@ .     * Elastic Beanstalk environment    * Specify the hosted zone ID for the region in which you created the environment. The environment must have a regionalized subdomain. For a list of regions and the corresponding hosted zone IDs, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region AWS Elastic Beanstalk> in the "AWS Regions and Endpoints" chapter of the /Amazon Web Services General Reference/ .     * ELB load balancer    * Specify the value of the hosted zone ID for the load balancer. Use the following methods to get the hosted zone ID:     * <http://docs.aws.amazon.com/general/latest/gr/rande.html#elb_region Elastic Load Balancing> table in the "AWS Regions and Endpoints" chapter of the /Amazon Web Services General Reference/ : Use the value that corresponds with the region that you created your load balancer in. Note that there are separate columns for Application and Classic Load Balancers and for Network Load Balancers.     * __AWS Management Console__ : Go to the Amazon EC2 page, choose __Load Balancers__ in the navigation pane, select the load balancer, and get the value of the __Hosted zone__ field on the __Description__ tab.     * __Elastic Load Balancing API__ : Use @DescribeLoadBalancers@ to get the applicable value. For more information, see the applicable guide:     * Classic Load Balancers: Use <http://docs.aws.amazon.com/elasticloadbalancing/2012-06-01/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers> to get the value of @CanonicalHostedZoneNameId@ .     * Application and Network Load Balancers: Use <http://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers> to get the value of @CanonicalHostedZoneId@ .     * __AWS CLI__ : Use @describe-load-balancers@ to get the applicable value. For more information, see the applicable guide:     * Classic Load Balancers: Use <http://docs.aws.amazon.com/cli/latest/reference/elb/describe-load-balancers.html describe-load-balancers> to get the value of @CanonicalHostedZoneNameId@ .     * Application and Network Load Balancers: Use <http://docs.aws.amazon.com/cli/latest/reference/elbv2/describe-load-balancers.html describe-load-balancers> to get the value of @CanonicalHostedZoneId@ .     * An Amazon S3 bucket configured as a static website    * Specify the hosted zone ID for the region that you created the bucket in. For more information about valid values, see the <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Amazon Simple Storage Service Website Endpoints> table in the "AWS Regions and Endpoints" chapter of the /Amazon Web Services General Reference/ .     * Another Amazon Route 53 resource record set in your hosted zone    * Specify the hosted zone ID of your hosted zone. (An alias resource record set can't reference a resource record set in a different hosted zone.)
--
-- * 'atDNSName' - /Alias resource record sets only:/ The value that you specify depends on where you want to route queries:     * CloudFront distribution    * Specify the domain name that CloudFront assigned when you created your distribution. Your CloudFront distribution must include an alternate domain name that matches the name of the resource record set. For example, if the name of the resource record set is /acme.example.com/ , your CloudFront distribution must include /acme.example.com/ as one of the alternate domain names. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/CNAMEs.html Using Alternate Domain Names (CNAMEs)> in the /Amazon CloudFront Developer Guide/ .     * Elastic Beanstalk environment    * Specify the @CNAME@ attribute for the environment. (The environment must have a regionalized domain name.) You can use the following methods to get the value of the CNAME attribute:     * /AWS Management Console/ : For information about how to get the value by using the console, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/customdomains.html Using Custom Domains with AWS Elastic Beanstalk> in the /AWS Elastic Beanstalk Developer Guide/ .     * /Elastic Beanstalk API/ : Use the @DescribeEnvironments@ action to get the value of the @CNAME@ attribute. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironments.html DescribeEnvironments> in the /AWS Elastic Beanstalk API Reference/ .     * /AWS CLI/ : Use the @describe-environments@ command to get the value of the @CNAME@ attribute. For more information, see <http://docs.aws.amazon.com/cli/latest/reference/elasticbeanstalk/describe-environments.html describe-environments> in the /AWS Command Line Interface Reference/ .     * ELB load balancer    * Specify the DNS name that is associated with the load balancer. Get the DNS name by using the AWS Management Console, the ELB API, or the AWS CLI.      * __AWS Management Console__ : Go to the EC2 page, choose __Load Balancers__ in the navigation pane, choose the load balancer, choose the __Description__ tab, and get the value of the __DNS name__ field. (If you're routing traffic to a Classic Load Balancer, get the value that begins with __dualstack__ .)      * __Elastic Load Balancing API__ : Use @DescribeLoadBalancers@ to get the value of @DNSName@ . For more information, see the applicable guide:     * Classic Load Balancers: <http://docs.aws.amazon.com/elasticloadbalancing/2012-06-01/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>      * Application and Network Load Balancers: <http://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>      * __AWS CLI__ : Use @describe-load-balancers@ to get the value of @DNSName@ . For more information, see the applicable guide:     * Classic Load Balancers: <http://docs.aws.amazon.com/cli/latest/reference/elb/describe-load-balancers.html describe-load-balancers>      * Application and Network Load Balancers: <http://docs.aws.amazon.com/cli/latest/reference/elbv2/describe-load-balancers.html describe-load-balancers>      * Amazon S3 bucket that is configured as a static website    * Specify the domain name of the Amazon S3 website endpoint in which you created the bucket, for example, @s3-website-us-east-2.amazonaws.com@ . For more information about valid values, see the table <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Amazon Simple Storage Service (S3) Website Endpoints> in the /Amazon Web Services General Reference/ . For more information about using S3 buckets for websites, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/getting-started.html Getting Started with Amazon Route 53> in the /Amazon Route 53 Developer Guide./      * Another Amazon Route 53 resource record set    * Specify the value of the @Name@ element for a resource record set in the current hosted zone.
--
-- * 'atEvaluateTargetHealth' - /Applies only to alias, failover alias, geolocation alias, latency alias, and weighted alias resource record sets:/ When @EvaluateTargetHealth@ is @true@ , an alias resource record set inherits the health of the referenced AWS resource, such as an ELB load balancer, or the referenced resource record set. Note the following:     * You can't set @EvaluateTargetHealth@ to @true@ when the alias target is a CloudFront distribution.     * If the AWS resource that you specify in @AliasTarget@ is a resource record set or a group of resource record sets (for example, a group of weighted resource record sets), but it is not another alias resource record set, we recommend that you associate a health check with all of the resource record sets in the alias target. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-complex-configs.html#dns-failover-complex-configs-hc-omitting What Happens When You Omit Health Checks?> in the /Amazon Route 53 Developer Guide/ .     * If you specify an Elastic Beanstalk environment in @HostedZoneId@ and @DNSName@ , and if the environment contains an ELB load balancer, Elastic Load Balancing routes queries only to the healthy Amazon EC2 instances that are registered with the load balancer. (An environment automatically contains an ELB load balancer if it includes more than one EC2 instance.) If you set @EvaluateTargetHealth@ to @true@ and either no EC2 instances are healthy or the load balancer itself is unhealthy, Amazon Route 53 routes queries to other available resources that are healthy, if any. If the environment contains a single EC2 instance, there are no special requirements.     * If you specify an ELB load balancer in @'AliasTarget' @ , ELB routes queries only to the healthy EC2 instances that are registered with the load balancer. If no EC2 instances are healthy or if the load balancer itself is unhealthy, and if @EvaluateTargetHealth@ is true for the corresponding alias resource record set, Amazon Route 53 routes queries to other resources. When you create a load balancer, you configure settings for ELB health checks; they're not Amazon Route 53 health checks, but they perform a similar function. Do not create Amazon Route 53 health checks for the EC2 instances that you register with an ELB load balancer. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-complex-configs.html How Health Checks Work in More Complex Amazon Route 53 Configurations> in the /Amazon Route 53 Developer Guide/ .     * We recommend that you set @EvaluateTargetHealth@ to true only when you have enough idle capacity to handle the failure of one or more endpoints. For more information and examples, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover> in the /Amazon Route 53 Developer Guide/ .
aliasTarget
    :: ResourceId -- ^ 'atHostedZoneId'
    -> Text -- ^ 'atDNSName'
    -> Bool -- ^ 'atEvaluateTargetHealth'
    -> AliasTarget
aliasTarget pHostedZoneId_ pDNSName_ pEvaluateTargetHealth_ =
  AliasTarget'
    { _atHostedZoneId = pHostedZoneId_
    , _atDNSName = pDNSName_
    , _atEvaluateTargetHealth = pEvaluateTargetHealth_
    }


-- | /Alias resource records sets only/ : The value used depends on where you want to route traffic:     * CloudFront distribution    * Specify @Z2FDTNDATAQYW2@ .     * Elastic Beanstalk environment    * Specify the hosted zone ID for the region in which you created the environment. The environment must have a regionalized subdomain. For a list of regions and the corresponding hosted zone IDs, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region AWS Elastic Beanstalk> in the "AWS Regions and Endpoints" chapter of the /Amazon Web Services General Reference/ .     * ELB load balancer    * Specify the value of the hosted zone ID for the load balancer. Use the following methods to get the hosted zone ID:     * <http://docs.aws.amazon.com/general/latest/gr/rande.html#elb_region Elastic Load Balancing> table in the "AWS Regions and Endpoints" chapter of the /Amazon Web Services General Reference/ : Use the value that corresponds with the region that you created your load balancer in. Note that there are separate columns for Application and Classic Load Balancers and for Network Load Balancers.     * __AWS Management Console__ : Go to the Amazon EC2 page, choose __Load Balancers__ in the navigation pane, select the load balancer, and get the value of the __Hosted zone__ field on the __Description__ tab.     * __Elastic Load Balancing API__ : Use @DescribeLoadBalancers@ to get the applicable value. For more information, see the applicable guide:     * Classic Load Balancers: Use <http://docs.aws.amazon.com/elasticloadbalancing/2012-06-01/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers> to get the value of @CanonicalHostedZoneNameId@ .     * Application and Network Load Balancers: Use <http://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers> to get the value of @CanonicalHostedZoneId@ .     * __AWS CLI__ : Use @describe-load-balancers@ to get the applicable value. For more information, see the applicable guide:     * Classic Load Balancers: Use <http://docs.aws.amazon.com/cli/latest/reference/elb/describe-load-balancers.html describe-load-balancers> to get the value of @CanonicalHostedZoneNameId@ .     * Application and Network Load Balancers: Use <http://docs.aws.amazon.com/cli/latest/reference/elbv2/describe-load-balancers.html describe-load-balancers> to get the value of @CanonicalHostedZoneId@ .     * An Amazon S3 bucket configured as a static website    * Specify the hosted zone ID for the region that you created the bucket in. For more information about valid values, see the <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Amazon Simple Storage Service Website Endpoints> table in the "AWS Regions and Endpoints" chapter of the /Amazon Web Services General Reference/ .     * Another Amazon Route 53 resource record set in your hosted zone    * Specify the hosted zone ID of your hosted zone. (An alias resource record set can't reference a resource record set in a different hosted zone.)
atHostedZoneId :: Lens' AliasTarget ResourceId
atHostedZoneId = lens _atHostedZoneId (\ s a -> s{_atHostedZoneId = a})

-- | /Alias resource record sets only:/ The value that you specify depends on where you want to route queries:     * CloudFront distribution    * Specify the domain name that CloudFront assigned when you created your distribution. Your CloudFront distribution must include an alternate domain name that matches the name of the resource record set. For example, if the name of the resource record set is /acme.example.com/ , your CloudFront distribution must include /acme.example.com/ as one of the alternate domain names. For more information, see <http://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/CNAMEs.html Using Alternate Domain Names (CNAMEs)> in the /Amazon CloudFront Developer Guide/ .     * Elastic Beanstalk environment    * Specify the @CNAME@ attribute for the environment. (The environment must have a regionalized domain name.) You can use the following methods to get the value of the CNAME attribute:     * /AWS Management Console/ : For information about how to get the value by using the console, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/customdomains.html Using Custom Domains with AWS Elastic Beanstalk> in the /AWS Elastic Beanstalk Developer Guide/ .     * /Elastic Beanstalk API/ : Use the @DescribeEnvironments@ action to get the value of the @CNAME@ attribute. For more information, see <http://docs.aws.amazon.com/elasticbeanstalk/latest/api/API_DescribeEnvironments.html DescribeEnvironments> in the /AWS Elastic Beanstalk API Reference/ .     * /AWS CLI/ : Use the @describe-environments@ command to get the value of the @CNAME@ attribute. For more information, see <http://docs.aws.amazon.com/cli/latest/reference/elasticbeanstalk/describe-environments.html describe-environments> in the /AWS Command Line Interface Reference/ .     * ELB load balancer    * Specify the DNS name that is associated with the load balancer. Get the DNS name by using the AWS Management Console, the ELB API, or the AWS CLI.      * __AWS Management Console__ : Go to the EC2 page, choose __Load Balancers__ in the navigation pane, choose the load balancer, choose the __Description__ tab, and get the value of the __DNS name__ field. (If you're routing traffic to a Classic Load Balancer, get the value that begins with __dualstack__ .)      * __Elastic Load Balancing API__ : Use @DescribeLoadBalancers@ to get the value of @DNSName@ . For more information, see the applicable guide:     * Classic Load Balancers: <http://docs.aws.amazon.com/elasticloadbalancing/2012-06-01/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>      * Application and Network Load Balancers: <http://docs.aws.amazon.com/elasticloadbalancing/latest/APIReference/API_DescribeLoadBalancers.html DescribeLoadBalancers>      * __AWS CLI__ : Use @describe-load-balancers@ to get the value of @DNSName@ . For more information, see the applicable guide:     * Classic Load Balancers: <http://docs.aws.amazon.com/cli/latest/reference/elb/describe-load-balancers.html describe-load-balancers>      * Application and Network Load Balancers: <http://docs.aws.amazon.com/cli/latest/reference/elbv2/describe-load-balancers.html describe-load-balancers>      * Amazon S3 bucket that is configured as a static website    * Specify the domain name of the Amazon S3 website endpoint in which you created the bucket, for example, @s3-website-us-east-2.amazonaws.com@ . For more information about valid values, see the table <http://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Amazon Simple Storage Service (S3) Website Endpoints> in the /Amazon Web Services General Reference/ . For more information about using S3 buckets for websites, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/getting-started.html Getting Started with Amazon Route 53> in the /Amazon Route 53 Developer Guide./      * Another Amazon Route 53 resource record set    * Specify the value of the @Name@ element for a resource record set in the current hosted zone.
atDNSName :: Lens' AliasTarget Text
atDNSName = lens _atDNSName (\ s a -> s{_atDNSName = a})

-- | /Applies only to alias, failover alias, geolocation alias, latency alias, and weighted alias resource record sets:/ When @EvaluateTargetHealth@ is @true@ , an alias resource record set inherits the health of the referenced AWS resource, such as an ELB load balancer, or the referenced resource record set. Note the following:     * You can't set @EvaluateTargetHealth@ to @true@ when the alias target is a CloudFront distribution.     * If the AWS resource that you specify in @AliasTarget@ is a resource record set or a group of resource record sets (for example, a group of weighted resource record sets), but it is not another alias resource record set, we recommend that you associate a health check with all of the resource record sets in the alias target. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-complex-configs.html#dns-failover-complex-configs-hc-omitting What Happens When You Omit Health Checks?> in the /Amazon Route 53 Developer Guide/ .     * If you specify an Elastic Beanstalk environment in @HostedZoneId@ and @DNSName@ , and if the environment contains an ELB load balancer, Elastic Load Balancing routes queries only to the healthy Amazon EC2 instances that are registered with the load balancer. (An environment automatically contains an ELB load balancer if it includes more than one EC2 instance.) If you set @EvaluateTargetHealth@ to @true@ and either no EC2 instances are healthy or the load balancer itself is unhealthy, Amazon Route 53 routes queries to other available resources that are healthy, if any. If the environment contains a single EC2 instance, there are no special requirements.     * If you specify an ELB load balancer in @'AliasTarget' @ , ELB routes queries only to the healthy EC2 instances that are registered with the load balancer. If no EC2 instances are healthy or if the load balancer itself is unhealthy, and if @EvaluateTargetHealth@ is true for the corresponding alias resource record set, Amazon Route 53 routes queries to other resources. When you create a load balancer, you configure settings for ELB health checks; they're not Amazon Route 53 health checks, but they perform a similar function. Do not create Amazon Route 53 health checks for the EC2 instances that you register with an ELB load balancer. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-complex-configs.html How Health Checks Work in More Complex Amazon Route 53 Configurations> in the /Amazon Route 53 Developer Guide/ .     * We recommend that you set @EvaluateTargetHealth@ to true only when you have enough idle capacity to handle the failure of one or more endpoints. For more information and examples, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover> in the /Amazon Route 53 Developer Guide/ .
atEvaluateTargetHealth :: Lens' AliasTarget Bool
atEvaluateTargetHealth = lens _atEvaluateTargetHealth (\ s a -> s{_atEvaluateTargetHealth = a})

instance FromXML AliasTarget where
        parseXML x
          = AliasTarget' <$>
              (x .@ "HostedZoneId") <*> (x .@ "DNSName") <*>
                (x .@ "EvaluateTargetHealth")

instance Hashable AliasTarget where

instance NFData AliasTarget where

instance ToXML AliasTarget where
        toXML AliasTarget'{..}
          = mconcat
              ["HostedZoneId" @= _atHostedZoneId,
               "DNSName" @= _atDNSName,
               "EvaluateTargetHealth" @= _atEvaluateTargetHealth]

-- | The information for each resource record set that you want to change.
--
--
--
-- /See:/ 'change' smart constructor.
data Change = Change'
  { _cAction            :: !ChangeAction
  , _cResourceRecordSet :: !ResourceRecordSet
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Change' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAction' - The action to perform:     * @CREATE@ : Creates a resource record set that has the specified values.     * @DELETE@ : Deletes a existing resource record set. /Important:/ To delete the resource record set that is associated with a traffic policy instance, use @'DeleteTrafficPolicyInstance' @ . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Amazon Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.      * @UPSERT@ : If a resource record set doesn't already exist, Amazon Route 53 creates it. If a resource record set does exist, Amazon Route 53 updates it with the values in the request.
--
-- * 'cResourceRecordSet' - Information about the resource record set to create, delete, or update.
change
    :: ChangeAction -- ^ 'cAction'
    -> ResourceRecordSet -- ^ 'cResourceRecordSet'
    -> Change
change pAction_ pResourceRecordSet_ =
  Change' {_cAction = pAction_, _cResourceRecordSet = pResourceRecordSet_}


-- | The action to perform:     * @CREATE@ : Creates a resource record set that has the specified values.     * @DELETE@ : Deletes a existing resource record set. /Important:/ To delete the resource record set that is associated with a traffic policy instance, use @'DeleteTrafficPolicyInstance' @ . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Amazon Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.      * @UPSERT@ : If a resource record set doesn't already exist, Amazon Route 53 creates it. If a resource record set does exist, Amazon Route 53 updates it with the values in the request.
cAction :: Lens' Change ChangeAction
cAction = lens _cAction (\ s a -> s{_cAction = a})

-- | Information about the resource record set to create, delete, or update.
cResourceRecordSet :: Lens' Change ResourceRecordSet
cResourceRecordSet = lens _cResourceRecordSet (\ s a -> s{_cResourceRecordSet = a})

instance Hashable Change where

instance NFData Change where

instance ToXML Change where
        toXML Change'{..}
          = mconcat
              ["Action" @= _cAction,
               "ResourceRecordSet" @= _cResourceRecordSet]

-- | The information for a change request.
--
--
--
-- /See:/ 'changeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
  { _cbComment :: !(Maybe Text)
  , _cbChanges :: !(List1 Change)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeBatch' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbComment' - /Optional:/ Any comments you want to include about a change batch request.
--
-- * 'cbChanges' - Information about the changes to make to the record sets.
changeBatch
    :: NonEmpty Change -- ^ 'cbChanges'
    -> ChangeBatch
changeBatch pChanges_ =
  ChangeBatch' {_cbComment = Nothing, _cbChanges = _List1 # pChanges_}


-- | /Optional:/ Any comments you want to include about a change batch request.
cbComment :: Lens' ChangeBatch (Maybe Text)
cbComment = lens _cbComment (\ s a -> s{_cbComment = a})

-- | Information about the changes to make to the record sets.
cbChanges :: Lens' ChangeBatch (NonEmpty Change)
cbChanges = lens _cbChanges (\ s a -> s{_cbChanges = a}) . _List1

instance Hashable ChangeBatch where

instance NFData ChangeBatch where

instance ToXML ChangeBatch where
        toXML ChangeBatch'{..}
          = mconcat
              ["Comment" @= _cbComment,
               "Changes" @= toXMLList "Change" _cbChanges]

-- | A complex type that describes change information about changes made to your hosted zone.
--
--
--
-- /See:/ 'changeInfo' smart constructor.
data ChangeInfo = ChangeInfo'
  { _ciComment     :: !(Maybe Text)
  , _ciId          :: !ResourceId
  , _ciStatus      :: !ChangeStatus
  , _ciSubmittedAt :: !ISO8601
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciComment' - A complex type that describes change information about changes made to your hosted zone. This element contains an ID that you use when performing a 'GetChange' action to get detailed information about the change.
--
-- * 'ciId' - The ID of the request.
--
-- * 'ciStatus' - The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
--
-- * 'ciSubmittedAt' - The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
changeInfo
    :: ResourceId -- ^ 'ciId'
    -> ChangeStatus -- ^ 'ciStatus'
    -> UTCTime -- ^ 'ciSubmittedAt'
    -> ChangeInfo
changeInfo pId_ pStatus_ pSubmittedAt_ =
  ChangeInfo'
    { _ciComment = Nothing
    , _ciId = pId_
    , _ciStatus = pStatus_
    , _ciSubmittedAt = _Time # pSubmittedAt_
    }


-- | A complex type that describes change information about changes made to your hosted zone. This element contains an ID that you use when performing a 'GetChange' action to get detailed information about the change.
ciComment :: Lens' ChangeInfo (Maybe Text)
ciComment = lens _ciComment (\ s a -> s{_ciComment = a})

-- | The ID of the request.
ciId :: Lens' ChangeInfo ResourceId
ciId = lens _ciId (\ s a -> s{_ciId = a})

-- | The current state of the request. @PENDING@ indicates that this request has not yet been applied to all Amazon Route 53 DNS servers.
ciStatus :: Lens' ChangeInfo ChangeStatus
ciStatus = lens _ciStatus (\ s a -> s{_ciStatus = a})

-- | The date and time that the change request was submitted in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
ciSubmittedAt :: Lens' ChangeInfo UTCTime
ciSubmittedAt = lens _ciSubmittedAt (\ s a -> s{_ciSubmittedAt = a}) . _Time

instance FromXML ChangeInfo where
        parseXML x
          = ChangeInfo' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*> (x .@ "Status")
                <*> (x .@ "SubmittedAt")

instance Hashable ChangeInfo where

instance NFData ChangeInfo where

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
--
--
-- /See:/ 'cloudWatchAlarmConfiguration' smart constructor.
data CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration'
  { _cwacDimensions         :: !(Maybe [Dimension])
  , _cwacEvaluationPeriods  :: !Nat
  , _cwacThreshold          :: !Double
  , _cwacComparisonOperator :: !ComparisonOperator
  , _cwacPeriod             :: !Nat
  , _cwacMetricName         :: !Text
  , _cwacNamespace          :: !Text
  , _cwacStatistic          :: !Statistic
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CloudWatchAlarmConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwacDimensions' - For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- * 'cwacEvaluationPeriods' - For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
--
-- * 'cwacThreshold' - For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
--
-- * 'cwacComparisonOperator' - For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
--
-- * 'cwacPeriod' - For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
--
-- * 'cwacMetricName' - The name of the CloudWatch metric that the alarm is associated with.
--
-- * 'cwacNamespace' - The namespace of the metric that the alarm is associated with. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
--
-- * 'cwacStatistic' - For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
cloudWatchAlarmConfiguration
    :: Natural -- ^ 'cwacEvaluationPeriods'
    -> Double -- ^ 'cwacThreshold'
    -> ComparisonOperator -- ^ 'cwacComparisonOperator'
    -> Natural -- ^ 'cwacPeriod'
    -> Text -- ^ 'cwacMetricName'
    -> Text -- ^ 'cwacNamespace'
    -> Statistic -- ^ 'cwacStatistic'
    -> CloudWatchAlarmConfiguration
cloudWatchAlarmConfiguration pEvaluationPeriods_ pThreshold_ pComparisonOperator_ pPeriod_ pMetricName_ pNamespace_ pStatistic_ =
  CloudWatchAlarmConfiguration'
    { _cwacDimensions = Nothing
    , _cwacEvaluationPeriods = _Nat # pEvaluationPeriods_
    , _cwacThreshold = pThreshold_
    , _cwacComparisonOperator = pComparisonOperator_
    , _cwacPeriod = _Nat # pPeriod_
    , _cwacMetricName = pMetricName_
    , _cwacNamespace = pNamespace_
    , _cwacStatistic = pStatistic_
    }


-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about the dimensions for the metric. For information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
cwacDimensions :: Lens' CloudWatchAlarmConfiguration [Dimension]
cwacDimensions = lens _cwacDimensions (\ s a -> s{_cwacDimensions = a}) . _Default . _Coerce

-- | For the metric that the CloudWatch alarm is associated with, the number of periods that the metric is compared to the threshold.
cwacEvaluationPeriods :: Lens' CloudWatchAlarmConfiguration Natural
cwacEvaluationPeriods = lens _cwacEvaluationPeriods (\ s a -> s{_cwacEvaluationPeriods = a}) . _Nat

-- | For the metric that the CloudWatch alarm is associated with, the value the metric is compared with.
cwacThreshold :: Lens' CloudWatchAlarmConfiguration Double
cwacThreshold = lens _cwacThreshold (\ s a -> s{_cwacThreshold = a})

-- | For the metric that the CloudWatch alarm is associated with, the arithmetic operation that is used for the comparison.
cwacComparisonOperator :: Lens' CloudWatchAlarmConfiguration ComparisonOperator
cwacComparisonOperator = lens _cwacComparisonOperator (\ s a -> s{_cwacComparisonOperator = a})

-- | For the metric that the CloudWatch alarm is associated with, the duration of one evaluation period in seconds.
cwacPeriod :: Lens' CloudWatchAlarmConfiguration Natural
cwacPeriod = lens _cwacPeriod (\ s a -> s{_cwacPeriod = a}) . _Nat

-- | The name of the CloudWatch metric that the alarm is associated with.
cwacMetricName :: Lens' CloudWatchAlarmConfiguration Text
cwacMetricName = lens _cwacMetricName (\ s a -> s{_cwacMetricName = a})

-- | The namespace of the metric that the alarm is associated with. For more information, see <http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/CW_Support_For_AWS.html Amazon CloudWatch Namespaces, Dimensions, and Metrics Reference> in the /Amazon CloudWatch User Guide/ .
cwacNamespace :: Lens' CloudWatchAlarmConfiguration Text
cwacNamespace = lens _cwacNamespace (\ s a -> s{_cwacNamespace = a})

-- | For the metric that the CloudWatch alarm is associated with, the statistic that is applied to the metric.
cwacStatistic :: Lens' CloudWatchAlarmConfiguration Statistic
cwacStatistic = lens _cwacStatistic (\ s a -> s{_cwacStatistic = a})

instance FromXML CloudWatchAlarmConfiguration where
        parseXML x
          = CloudWatchAlarmConfiguration' <$>
              (x .@? "Dimensions" .!@ mempty >>=
                 may (parseXMLList "Dimension"))
                <*> (x .@ "EvaluationPeriods")
                <*> (x .@ "Threshold")
                <*> (x .@ "ComparisonOperator")
                <*> (x .@ "Period")
                <*> (x .@ "MetricName")
                <*> (x .@ "Namespace")
                <*> (x .@ "Statistic")

instance Hashable CloudWatchAlarmConfiguration where

instance NFData CloudWatchAlarmConfiguration where

-- | A complex type that lists the name servers in a delegation set, as well as the @CallerReference@ and the @ID@ for the delegation set.
--
--
--
-- /See:/ 'delegationSet' smart constructor.
data DelegationSet = DelegationSet'
  { _dsId              :: !(Maybe ResourceId)
  , _dsCallerReference :: !(Maybe Text)
  , _dsNameServers     :: !(List1 Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DelegationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsId' - The ID that Amazon Route 53 assigns to a reusable delegation set.
--
-- * 'dsCallerReference' - The value that you specified for @CallerReference@ when you created the reusable delegation set.
--
-- * 'dsNameServers' - A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
delegationSet
    :: NonEmpty Text -- ^ 'dsNameServers'
    -> DelegationSet
delegationSet pNameServers_ =
  DelegationSet'
    { _dsId = Nothing
    , _dsCallerReference = Nothing
    , _dsNameServers = _List1 # pNameServers_
    }


-- | The ID that Amazon Route 53 assigns to a reusable delegation set.
dsId :: Lens' DelegationSet (Maybe ResourceId)
dsId = lens _dsId (\ s a -> s{_dsId = a})

-- | The value that you specified for @CallerReference@ when you created the reusable delegation set.
dsCallerReference :: Lens' DelegationSet (Maybe Text)
dsCallerReference = lens _dsCallerReference (\ s a -> s{_dsCallerReference = a})

-- | A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
dsNameServers :: Lens' DelegationSet (NonEmpty Text)
dsNameServers = lens _dsNameServers (\ s a -> s{_dsNameServers = a}) . _List1

instance FromXML DelegationSet where
        parseXML x
          = DelegationSet' <$>
              (x .@? "Id") <*> (x .@? "CallerReference") <*>
                (x .@? "NameServers" .!@ mempty >>=
                   parseXMLList1 "NameServer")

instance Hashable DelegationSet where

instance NFData DelegationSet where

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.
--
--
--
-- /See:/ 'dimension' smart constructor.
data Dimension = Dimension'
  { _dName  :: !Text
  , _dValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Dimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - For the metric that the CloudWatch alarm is associated with, the name of one dimension.
--
-- * 'dValue' - For the metric that the CloudWatch alarm is associated with, the value of one dimension.
dimension
    :: Text -- ^ 'dName'
    -> Text -- ^ 'dValue'
    -> Dimension
dimension pName_ pValue_ = Dimension' {_dName = pName_, _dValue = pValue_}


-- | For the metric that the CloudWatch alarm is associated with, the name of one dimension.
dName :: Lens' Dimension Text
dName = lens _dName (\ s a -> s{_dName = a})

-- | For the metric that the CloudWatch alarm is associated with, the value of one dimension.
dValue :: Lens' Dimension Text
dValue = lens _dValue (\ s a -> s{_dValue = a})

instance FromXML Dimension where
        parseXML x
          = Dimension' <$> (x .@ "Name") <*> (x .@ "Value")

instance Hashable Dimension where

instance NFData Dimension where

-- | A complex type that contains information about a geo location.
--
--
--
-- /See:/ 'geoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { _glSubdivisionCode :: !(Maybe Text)
  , _glCountryCode     :: !(Maybe Text)
  , _glContinentCode   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glSubdivisionCode' - The code for the subdivision, for example, a state in the United States or a province in Canada.
--
-- * 'glCountryCode' - The two-letter code for the country.
--
-- * 'glContinentCode' - The two-letter code for the continent. Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@  Constraint: Specifying @ContinentCode@ with either @CountryCode@ or @SubdivisionCode@ returns an @InvalidInput@ error.
geoLocation
    :: GeoLocation
geoLocation =
  GeoLocation'
    { _glSubdivisionCode = Nothing
    , _glCountryCode = Nothing
    , _glContinentCode = Nothing
    }


-- | The code for the subdivision, for example, a state in the United States or a province in Canada.
glSubdivisionCode :: Lens' GeoLocation (Maybe Text)
glSubdivisionCode = lens _glSubdivisionCode (\ s a -> s{_glSubdivisionCode = a})

-- | The two-letter code for the country.
glCountryCode :: Lens' GeoLocation (Maybe Text)
glCountryCode = lens _glCountryCode (\ s a -> s{_glCountryCode = a})

-- | The two-letter code for the continent. Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@  Constraint: Specifying @ContinentCode@ with either @CountryCode@ or @SubdivisionCode@ returns an @InvalidInput@ error.
glContinentCode :: Lens' GeoLocation (Maybe Text)
glContinentCode = lens _glContinentCode (\ s a -> s{_glContinentCode = a})

instance FromXML GeoLocation where
        parseXML x
          = GeoLocation' <$>
              (x .@? "SubdivisionCode") <*> (x .@? "CountryCode")
                <*> (x .@? "ContinentCode")

instance Hashable GeoLocation where

instance NFData GeoLocation where

instance ToXML GeoLocation where
        toXML GeoLocation'{..}
          = mconcat
              ["SubdivisionCode" @= _glSubdivisionCode,
               "CountryCode" @= _glCountryCode,
               "ContinentCode" @= _glContinentCode]

-- | A complex type that contains the codes and full continent, country, and subdivision names for the specified @geolocation@ code.
--
--
--
-- /See:/ 'geoLocationDetails' smart constructor.
data GeoLocationDetails = GeoLocationDetails'
  { _gldSubdivisionName :: !(Maybe Text)
  , _gldSubdivisionCode :: !(Maybe Text)
  , _gldCountryName     :: !(Maybe Text)
  , _gldCountryCode     :: !(Maybe Text)
  , _gldContinentCode   :: !(Maybe Text)
  , _gldContinentName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GeoLocationDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gldSubdivisionName' - The full name of the subdivision, for example, a state in the United States or a province in Canada.
--
-- * 'gldSubdivisionCode' - The code for the subdivision, for example, a state in the United States or a province in Canada.
--
-- * 'gldCountryName' - The name of the country.
--
-- * 'gldCountryCode' - The two-letter code for the country.
--
-- * 'gldContinentCode' - The two-letter code for the continent.
--
-- * 'gldContinentName' - The full name of the continent.
geoLocationDetails
    :: GeoLocationDetails
geoLocationDetails =
  GeoLocationDetails'
    { _gldSubdivisionName = Nothing
    , _gldSubdivisionCode = Nothing
    , _gldCountryName = Nothing
    , _gldCountryCode = Nothing
    , _gldContinentCode = Nothing
    , _gldContinentName = Nothing
    }


-- | The full name of the subdivision, for example, a state in the United States or a province in Canada.
gldSubdivisionName :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionName = lens _gldSubdivisionName (\ s a -> s{_gldSubdivisionName = a})

-- | The code for the subdivision, for example, a state in the United States or a province in Canada.
gldSubdivisionCode :: Lens' GeoLocationDetails (Maybe Text)
gldSubdivisionCode = lens _gldSubdivisionCode (\ s a -> s{_gldSubdivisionCode = a})

-- | The name of the country.
gldCountryName :: Lens' GeoLocationDetails (Maybe Text)
gldCountryName = lens _gldCountryName (\ s a -> s{_gldCountryName = a})

-- | The two-letter code for the country.
gldCountryCode :: Lens' GeoLocationDetails (Maybe Text)
gldCountryCode = lens _gldCountryCode (\ s a -> s{_gldCountryCode = a})

-- | The two-letter code for the continent.
gldContinentCode :: Lens' GeoLocationDetails (Maybe Text)
gldContinentCode = lens _gldContinentCode (\ s a -> s{_gldContinentCode = a})

-- | The full name of the continent.
gldContinentName :: Lens' GeoLocationDetails (Maybe Text)
gldContinentName = lens _gldContinentName (\ s a -> s{_gldContinentName = a})

instance FromXML GeoLocationDetails where
        parseXML x
          = GeoLocationDetails' <$>
              (x .@? "SubdivisionName") <*>
                (x .@? "SubdivisionCode")
                <*> (x .@? "CountryName")
                <*> (x .@? "CountryCode")
                <*> (x .@? "ContinentCode")
                <*> (x .@? "ContinentName")

instance Hashable GeoLocationDetails where

instance NFData GeoLocationDetails where

-- | A complex type that contains information about one health check that is associated with the current AWS account.
--
--
--
-- /See:/ 'healthCheck' smart constructor.
data HealthCheck = HealthCheck'
  { _hcLinkedService                :: !(Maybe LinkedService)
  , _hcCloudWatchAlarmConfiguration :: !(Maybe CloudWatchAlarmConfiguration)
  , _hcId                           :: !Text
  , _hcCallerReference              :: !Text
  , _hcHealthCheckConfig            :: !HealthCheckConfig
  , _hcHealthCheckVersion           :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheck' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcLinkedService' - If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- * 'hcCloudWatchAlarmConfiguration' - A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
--
-- * 'hcId' - The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
--
-- * 'hcCallerReference' - A unique string that you specified when you created the health check.
--
-- * 'hcHealthCheckConfig' - A complex type that contains detailed information about one health check.
--
-- * 'hcHealthCheckVersion' - The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
healthCheck
    :: Text -- ^ 'hcId'
    -> Text -- ^ 'hcCallerReference'
    -> HealthCheckConfig -- ^ 'hcHealthCheckConfig'
    -> Natural -- ^ 'hcHealthCheckVersion'
    -> HealthCheck
healthCheck pId_ pCallerReference_ pHealthCheckConfig_ pHealthCheckVersion_ =
  HealthCheck'
    { _hcLinkedService = Nothing
    , _hcCloudWatchAlarmConfiguration = Nothing
    , _hcId = pId_
    , _hcCallerReference = pCallerReference_
    , _hcHealthCheckConfig = pHealthCheckConfig_
    , _hcHealthCheckVersion = _Nat # pHealthCheckVersion_
    }


-- | If the health check was created by another service, the service that created the health check. When a health check is created by another service, you can't edit or delete it using Amazon Route 53.
hcLinkedService :: Lens' HealthCheck (Maybe LinkedService)
hcLinkedService = lens _hcLinkedService (\ s a -> s{_hcLinkedService = a})

-- | A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.
hcCloudWatchAlarmConfiguration :: Lens' HealthCheck (Maybe CloudWatchAlarmConfiguration)
hcCloudWatchAlarmConfiguration = lens _hcCloudWatchAlarmConfiguration (\ s a -> s{_hcCloudWatchAlarmConfiguration = a})

-- | The identifier that Amazon Route 53assigned to the health check when you created it. When you add or update a resource record set, you use this value to specify which health check to use. The value can be up to 64 characters long.
hcId :: Lens' HealthCheck Text
hcId = lens _hcId (\ s a -> s{_hcId = a})

-- | A unique string that you specified when you created the health check.
hcCallerReference :: Lens' HealthCheck Text
hcCallerReference = lens _hcCallerReference (\ s a -> s{_hcCallerReference = a})

-- | A complex type that contains detailed information about one health check.
hcHealthCheckConfig :: Lens' HealthCheck HealthCheckConfig
hcHealthCheckConfig = lens _hcHealthCheckConfig (\ s a -> s{_hcHealthCheckConfig = a})

-- | The version of the health check. You can optionally pass this value in a call to @UpdateHealthCheck@ to prevent overwriting another change to the health check.
hcHealthCheckVersion :: Lens' HealthCheck Natural
hcHealthCheckVersion = lens _hcHealthCheckVersion (\ s a -> s{_hcHealthCheckVersion = a}) . _Nat

instance FromXML HealthCheck where
        parseXML x
          = HealthCheck' <$>
              (x .@? "LinkedService") <*>
                (x .@? "CloudWatchAlarmConfiguration")
                <*> (x .@ "Id")
                <*> (x .@ "CallerReference")
                <*> (x .@ "HealthCheckConfig")
                <*> (x .@ "HealthCheckVersion")

instance Hashable HealthCheck where

instance NFData HealthCheck where

-- | A complex type that contains information about the health check.
--
--
--
-- /See:/ 'healthCheckConfig' smart constructor.
data HealthCheckConfig = HealthCheckConfig'
  { _hccFailureThreshold             :: !(Maybe Nat)
  , _hccIPAddress                    :: !(Maybe Text)
  , _hccEnableSNI                    :: !(Maybe Bool)
  , _hccSearchString                 :: !(Maybe Text)
  , _hccHealthThreshold              :: !(Maybe Nat)
  , _hccRegions                      :: !(Maybe (List1 HealthCheckRegion))
  , _hccResourcePath                 :: !(Maybe Text)
  , _hccInsufficientDataHealthStatus :: !(Maybe InsufficientDataHealthStatus)
  , _hccAlarmIdentifier              :: !(Maybe AlarmIdentifier)
  , _hccMeasureLatency               :: !(Maybe Bool)
  , _hccInverted                     :: !(Maybe Bool)
  , _hccFullyQualifiedDomainName     :: !(Maybe Text)
  , _hccChildHealthChecks            :: !(Maybe [Text])
  , _hccRequestInterval              :: !(Maybe Nat)
  , _hccPort                         :: !(Maybe Nat)
  , _hccType                         :: !HealthCheckType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheckConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hccFailureThreshold' - The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ . If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
--
-- * 'hccIPAddress' - The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Amazon Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Amazon Route 53 then checks the health of the endpoint. Use one of the following formats for the value of @IPAddress@ :      * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ . If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change. For more information, see 'HealthCheckConfig$FullyQualifiedDomainName' . Constraints: Amazon Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>      * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>      * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>  When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
--
-- * 'hccEnableSNI' - Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate. Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid. The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
--
-- * 'hccSearchString' - If the value of Type is @HTTP_STR_MATCH@ or @HTTP_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Amazon Route 53 considers the resource healthy. Amazon Route 53 considers case when searching for @SearchString@ in the response body.
--
-- * 'hccHealthThreshold' - The number of child health checks that are associated with a @CALCULATED@ health that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the 'HealthCheckConfig$ChildHealthChecks' and 'HealthCheckConfig$ChildHealthChecks' elements. Note the following:     * If you specify a number greater than the number of child health checks, Amazon Route 53 always considers this health check to be unhealthy.     * If you specify @0@ , Amazon Route 53 always considers this health check to be healthy.
--
-- * 'hccRegions' - A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint. If you don't specify any regions, Amazon Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ . If you update a health check to remove a region that has been performing health checks, Amazon Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
--
-- * 'hccResourcePath' - The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html.
--
-- * 'hccInsufficientDataHealthStatus' - When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:     * @Healthy@ : Amazon Route 53 considers the health check to be healthy.     * @Unhealthy@ : Amazon Route 53 considers the health check to be unhealthy.     * @LastKnownStatus@ : Amazon Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
--
-- * 'hccAlarmIdentifier' - A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
--
-- * 'hccMeasureLatency' - Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Amazon Route 53 console. /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
--
-- * 'hccInverted' - Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
--
-- * 'hccFullyQualifiedDomainName' - Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ . __If you specify a value for__ @IPAddress@ : Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Amazon Route 53 to perform health checks. When Amazon Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Amazon Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.      * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Amazon Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Amazon Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header. If you don't specify a value for @FullyQualifiedDomainName@ , Amazon Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases. __If you don't specify a value for @IPAddress@ __ : Amazon Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Amazon Route 53 then checks the health of the endpoint. If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com). /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable. In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Amazon Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Amazon Route 53 doesn't pass a @Host@ header.
--
-- * 'hccChildHealthChecks' - (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
--
-- * 'hccRequestInterval' - The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Amazon Route 53 health checker makes requests at this interval. /Important:/ You can't change the value of @RequestInterval@ after you create a health check. If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
--
-- * 'hccPort' - The port on the endpoint on which you want Amazon Route 53 to perform health checks. Specify a value for @Port@ only when you specify a value for @IPAddress@ .
--
-- * 'hccType' - The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __HTTP_STR_MATCH__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __HTTPS_STR_MATCH__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __TCP__ : Amazon Route 53 tries to establish a TCP connection.     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .      * __CALCULATED__ : For health checks that monitor the status of other health checks, Amazon Route 53 adds up the number of health checks that Amazon Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .  For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
healthCheckConfig
    :: HealthCheckType -- ^ 'hccType'
    -> HealthCheckConfig
healthCheckConfig pType_ =
  HealthCheckConfig'
    { _hccFailureThreshold = Nothing
    , _hccIPAddress = Nothing
    , _hccEnableSNI = Nothing
    , _hccSearchString = Nothing
    , _hccHealthThreshold = Nothing
    , _hccRegions = Nothing
    , _hccResourcePath = Nothing
    , _hccInsufficientDataHealthStatus = Nothing
    , _hccAlarmIdentifier = Nothing
    , _hccMeasureLatency = Nothing
    , _hccInverted = Nothing
    , _hccFullyQualifiedDomainName = Nothing
    , _hccChildHealthChecks = Nothing
    , _hccRequestInterval = Nothing
    , _hccPort = Nothing
    , _hccType = pType_
    }


-- | The number of consecutive health checks that an endpoint must pass or fail for Amazon Route 53 to change the current status of the endpoint from unhealthy to healthy or vice versa. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ . If you don't specify a value for @FailureThreshold@ , the default value is three health checks.
hccFailureThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccFailureThreshold = lens _hccFailureThreshold (\ s a -> s{_hccFailureThreshold = a}) . mapping _Nat

-- | The IPv4 or IPv6 IP address of the endpoint that you want Amazon Route 53 to perform health checks on. If you don't specify a value for @IPAddress@ , Amazon Route 53 sends a DNS request to resolve the domain name that you specify in @FullyQualifiedDomainName@ at the interval that you specify in @RequestInterval@ . Using an IP address returned by DNS, Amazon Route 53 then checks the health of the endpoint. Use one of the following formats for the value of @IPAddress@ :      * __IPv4 address__ : four values between 0 and 255, separated by periods (.), for example, @192.0.2.44@ .     * __IPv6 address__ : eight groups of four hexadecimal values, separated by colons (:), for example, @2001:0db8:85a3:0000:0000:abcd:0001:2345@ . You can also shorten IPv6 addresses as described in RFC 5952, for example, @2001:db8:85a3::abcd:1:2345@ . If the endpoint is an EC2 instance, we recommend that you create an Elastic IP address, associate it with your EC2 instance, and specify the Elastic IP address for @IPAddress@ . This ensures that the IP address of your instance will never change. For more information, see 'HealthCheckConfig$FullyQualifiedDomainName' . Constraints: Amazon Route 53 can't check the health of endpoints for which the IP address is in local, private, non-routable, or multicast ranges. For more information about IP addresses for which you can't create health checks, see the following documents:     * <https://tools.ietf.org/html/rfc5735 RFC 5735, Special Use IPv4 Addresses>      * <https://tools.ietf.org/html/rfc6598 RFC 6598, IANA-Reserved IPv4 Prefix for Shared Address Space>      * <https://tools.ietf.org/html/rfc5156 RFC 5156, Special-Use IPv6 Addresses>  When the value of @Type@ is @CALCULATED@ or @CLOUDWATCH_METRIC@ , omit @IPAddress@ .
hccIPAddress :: Lens' HealthCheckConfig (Maybe Text)
hccIPAddress = lens _hccIPAddress (\ s a -> s{_hccIPAddress = a})

-- | Specify whether you want Amazon Route 53 to send the value of @FullyQualifiedDomainName@ to the endpoint in the @client_hello@ message during TLS negotiation. This allows the endpoint to respond to @HTTPS@ health check requests with the applicable SSL/TLS certificate. Some endpoints require that @HTTPS@ requests include the host name in the @client_hello@ message. If you don't enable SNI, the status of the health check will be @SSL alert handshake_failure@ . A health check can also have that status for other reasons. If SNI is enabled and you're still getting the error, check the SSL/TLS configuration on your endpoint and confirm that your certificate is valid. The SSL/TLS certificate on your endpoint includes a domain name in the @Common Name@ field and possibly several more in the @Subject Alternative Names@ field. One of the domain names in the certificate should match the value that you specify for @FullyQualifiedDomainName@ . If the endpoint responds to the @client_hello@ message with a certificate that does not include the domain name that you specified in @FullyQualifiedDomainName@ , a health checker will retry the handshake. In the second attempt, the health checker will omit @FullyQualifiedDomainName@ from the @client_hello@ message.
hccEnableSNI :: Lens' HealthCheckConfig (Maybe Bool)
hccEnableSNI = lens _hccEnableSNI (\ s a -> s{_hccEnableSNI = a})

-- | If the value of Type is @HTTP_STR_MATCH@ or @HTTP_STR_MATCH@ , the string that you want Amazon Route 53 to search for in the response body from the specified resource. If the string appears in the response body, Amazon Route 53 considers the resource healthy. Amazon Route 53 considers case when searching for @SearchString@ in the response body.
hccSearchString :: Lens' HealthCheckConfig (Maybe Text)
hccSearchString = lens _hccSearchString (\ s a -> s{_hccSearchString = a})

-- | The number of child health checks that are associated with a @CALCULATED@ health that Amazon Route 53 must consider healthy for the @CALCULATED@ health check to be considered healthy. To specify the child health checks that you want to associate with a @CALCULATED@ health check, use the 'HealthCheckConfig$ChildHealthChecks' and 'HealthCheckConfig$ChildHealthChecks' elements. Note the following:     * If you specify a number greater than the number of child health checks, Amazon Route 53 always considers this health check to be unhealthy.     * If you specify @0@ , Amazon Route 53 always considers this health check to be healthy.
hccHealthThreshold :: Lens' HealthCheckConfig (Maybe Natural)
hccHealthThreshold = lens _hccHealthThreshold (\ s a -> s{_hccHealthThreshold = a}) . mapping _Nat

-- | A complex type that contains one @Region@ element for each region from which you want Amazon Route 53 health checkers to check the specified endpoint. If you don't specify any regions, Amazon Route 53 health checkers automatically performs checks from all of the regions that are listed under __Valid Values__ . If you update a health check to remove a region that has been performing health checks, Amazon Route 53 will briefly continue to perform checks from that region to ensure that some health checkers are always checking the endpoint (for example, if you replace three regions with four different regions).
hccRegions :: Lens' HealthCheckConfig (Maybe (NonEmpty HealthCheckRegion))
hccRegions = lens _hccRegions (\ s a -> s{_hccRegions = a}) . mapping _List1

-- | The path, if any, that you want Amazon Route 53 to request when performing health checks. The path can be any value for which your endpoint will return an HTTP status code of 2xx or 3xx when the endpoint is healthy, for example, the file /docs/route53-health-check.html.
hccResourcePath :: Lens' HealthCheckConfig (Maybe Text)
hccResourcePath = lens _hccResourcePath (\ s a -> s{_hccResourcePath = a})

-- | When CloudWatch has insufficient data about the metric to determine the alarm state, the status that you want Amazon Route 53 to assign to the health check:     * @Healthy@ : Amazon Route 53 considers the health check to be healthy.     * @Unhealthy@ : Amazon Route 53 considers the health check to be unhealthy.     * @LastKnownStatus@ : Amazon Route 53 uses the status of the health check from the last time that CloudWatch had sufficient data to determine the alarm state. For new health checks that have no last known status, the default status for the health check is healthy.
hccInsufficientDataHealthStatus :: Lens' HealthCheckConfig (Maybe InsufficientDataHealthStatus)
hccInsufficientDataHealthStatus = lens _hccInsufficientDataHealthStatus (\ s a -> s{_hccInsufficientDataHealthStatus = a})

-- | A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.
hccAlarmIdentifier :: Lens' HealthCheckConfig (Maybe AlarmIdentifier)
hccAlarmIdentifier = lens _hccAlarmIdentifier (\ s a -> s{_hccAlarmIdentifier = a})

-- | Specify whether you want Amazon Route 53 to measure the latency between health checkers in multiple AWS regions and your endpoint, and to display CloudWatch latency graphs on the __Health Checks__ page in the Amazon Route 53 console. /Important:/ You can't change the value of @MeasureLatency@ after you create a health check.
hccMeasureLatency :: Lens' HealthCheckConfig (Maybe Bool)
hccMeasureLatency = lens _hccMeasureLatency (\ s a -> s{_hccMeasureLatency = a})

-- | Specify whether you want Amazon Route 53 to invert the status of a health check, for example, to consider a health check unhealthy when it otherwise would be considered healthy.
hccInverted :: Lens' HealthCheckConfig (Maybe Bool)
hccInverted = lens _hccInverted (\ s a -> s{_hccInverted = a})

-- | Amazon Route 53 behavior depends on whether you specify a value for @IPAddress@ . __If you specify a value for__ @IPAddress@ : Amazon Route 53 sends health check requests to the specified IPv4 or IPv6 address and passes the value of @FullyQualifiedDomainName@ in the @Host@ header for all health checks except TCP health checks. This is typically the fully qualified DNS name of the endpoint on which you want Amazon Route 53 to perform health checks. When Amazon Route 53 checks the health of an endpoint, here is how it constructs the @Host@ header:     * If you specify a value of @80@ for @Port@ and @HTTP@ or @HTTP_STR_MATCH@ for @Type@ , Amazon Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the Host header.      * If you specify a value of @443@ for @Port@ and @HTTPS@ or @HTTPS_STR_MATCH@ for @Type@ , Amazon Route 53 passes the value of @FullyQualifiedDomainName@ to the endpoint in the @Host@ header.     * If you specify another value for @Port@ and any value except @TCP@ for @Type@ , Amazon Route 53 passes @FullyQualifiedDomainName:Port@ to the endpoint in the @Host@ header. If you don't specify a value for @FullyQualifiedDomainName@ , Amazon Route 53 substitutes the value of @IPAddress@ in the @Host@ header in each of the preceding cases. __If you don't specify a value for @IPAddress@ __ : Amazon Route 53 sends a DNS request to the domain that you specify for @FullyQualifiedDomainName@ at the interval that you specify for @RequestInterval@ . Using an IPv4 address that DNS returns, Amazon Route 53 then checks the health of the endpoint. If you want to check the health of weighted, latency, or failover resource record sets and you choose to specify the endpoint only by @FullyQualifiedDomainName@ , we recommend that you create a separate health check for each endpoint. For example, create a health check for each HTTP server that is serving content for www.example.com. For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as us-east-2-www.example.com), not the name of the resource record sets (www.example.com). /Important:/ In this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and you then associate the health check with those resource record sets, health check results will be unpredictable. In addition, if the value that you specify for @Type@ is @HTTP@ , @HTTPS@ , @HTTP_STR_MATCH@ , or @HTTPS_STR_MATCH@ , Amazon Route 53 passes the value of @FullyQualifiedDomainName@ in the @Host@ header, as it does when you specify a value for @IPAddress@ . If the value of @Type@ is @TCP@ , Amazon Route 53 doesn't pass a @Host@ header.
hccFullyQualifiedDomainName :: Lens' HealthCheckConfig (Maybe Text)
hccFullyQualifiedDomainName = lens _hccFullyQualifiedDomainName (\ s a -> s{_hccFullyQualifiedDomainName = a})

-- | (CALCULATED Health Checks Only) A complex type that contains one @ChildHealthCheck@ element for each health check that you want to associate with a @CALCULATED@ health check.
hccChildHealthChecks :: Lens' HealthCheckConfig [Text]
hccChildHealthChecks = lens _hccChildHealthChecks (\ s a -> s{_hccChildHealthChecks = a}) . _Default . _Coerce

-- | The number of seconds between the time that Amazon Route 53 gets a response from your endpoint and the time that it sends the next health check request. Each Amazon Route 53 health checker makes requests at this interval. /Important:/ You can't change the value of @RequestInterval@ after you create a health check. If you don't specify a value for @RequestInterval@ , the default value is @30@ seconds.
hccRequestInterval :: Lens' HealthCheckConfig (Maybe Natural)
hccRequestInterval = lens _hccRequestInterval (\ s a -> s{_hccRequestInterval = a}) . mapping _Nat

-- | The port on the endpoint on which you want Amazon Route 53 to perform health checks. Specify a value for @Port@ only when you specify a value for @IPAddress@ .
hccPort :: Lens' HealthCheckConfig (Maybe Natural)
hccPort = lens _hccPort (\ s a -> s{_hccPort = a}) . mapping _Nat

-- | The type of health check that you want to create, which indicates how Amazon Route 53 determines whether an endpoint is healthy. /Important:/ You can't change the value of @Type@ after you create a health check. You can create the following types of health checks:     * __HTTP__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an HTTP request and waits for an HTTP status code of 200 or greater and less than 400.     * __HTTPS__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an HTTPS request and waits for an HTTP status code of 200 or greater and less than 400. /Important:/ If you specify @HTTPS@ for the value of @Type@ , the endpoint must support TLS v1.0 or later.     * __HTTP_STR_MATCH__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an HTTP request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __HTTPS_STR_MATCH__ : Amazon Route 53 tries to establish a TCP connection. If successful, Amazon Route 53 submits an @HTTPS@ request and searches the first 5,120 bytes of the response body for the string that you specify in @SearchString@ .     * __TCP__ : Amazon Route 53 tries to establish a TCP connection.     * __CLOUDWATCH_METRIC__ : The health check is associated with a CloudWatch alarm. If the state of the alarm is @OK@ , the health check is considered healthy. If the state is @ALARM@ , the health check is considered unhealthy. If CloudWatch doesn't have sufficient data to determine whether the state is @OK@ or @ALARM@ , the health check status depends on the setting for @InsufficientDataHealthStatus@ : @Healthy@ , @Unhealthy@ , or @LastKnownStatus@ .      * __CALCULATED__ : For health checks that monitor the status of other health checks, Amazon Route 53 adds up the number of health checks that Amazon Route 53 health checkers consider to be healthy and compares that number with the value of @HealthThreshold@ .  For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> in the /Amazon Route 53 Developer Guide/ .
hccType :: Lens' HealthCheckConfig HealthCheckType
hccType = lens _hccType (\ s a -> s{_hccType = a})

instance FromXML HealthCheckConfig where
        parseXML x
          = HealthCheckConfig' <$>
              (x .@? "FailureThreshold") <*> (x .@? "IPAddress")
                <*> (x .@? "EnableSNI")
                <*> (x .@? "SearchString")
                <*> (x .@? "HealthThreshold")
                <*>
                (x .@? "Regions" .!@ mempty >>=
                   may (parseXMLList1 "Region"))
                <*> (x .@? "ResourcePath")
                <*> (x .@? "InsufficientDataHealthStatus")
                <*> (x .@? "AlarmIdentifier")
                <*> (x .@? "MeasureLatency")
                <*> (x .@? "Inverted")
                <*> (x .@? "FullyQualifiedDomainName")
                <*>
                (x .@? "ChildHealthChecks" .!@ mempty >>=
                   may (parseXMLList "ChildHealthCheck"))
                <*> (x .@? "RequestInterval")
                <*> (x .@? "Port")
                <*> (x .@ "Type")

instance Hashable HealthCheckConfig where

instance NFData HealthCheckConfig where

instance ToXML HealthCheckConfig where
        toXML HealthCheckConfig'{..}
          = mconcat
              ["FailureThreshold" @= _hccFailureThreshold,
               "IPAddress" @= _hccIPAddress,
               "EnableSNI" @= _hccEnableSNI,
               "SearchString" @= _hccSearchString,
               "HealthThreshold" @= _hccHealthThreshold,
               "Regions" @=
                 toXML (toXMLList "Region" <$> _hccRegions),
               "ResourcePath" @= _hccResourcePath,
               "InsufficientDataHealthStatus" @=
                 _hccInsufficientDataHealthStatus,
               "AlarmIdentifier" @= _hccAlarmIdentifier,
               "MeasureLatency" @= _hccMeasureLatency,
               "Inverted" @= _hccInverted,
               "FullyQualifiedDomainName" @=
                 _hccFullyQualifiedDomainName,
               "ChildHealthChecks" @=
                 toXML
                   (toXMLList "ChildHealthCheck" <$>
                      _hccChildHealthChecks),
               "RequestInterval" @= _hccRequestInterval,
               "Port" @= _hccPort, "Type" @= _hccType]

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.
--
--
--
-- /See:/ 'healthCheckObservation' smart constructor.
data HealthCheckObservation = HealthCheckObservation'
  { _hcoIPAddress    :: !(Maybe Text)
  , _hcoStatusReport :: !(Maybe StatusReport)
  , _hcoRegion       :: !(Maybe HealthCheckRegion)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HealthCheckObservation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcoIPAddress' - The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
--
-- * 'hcoStatusReport' - A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
--
-- * 'hcoRegion' - The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
healthCheckObservation
    :: HealthCheckObservation
healthCheckObservation =
  HealthCheckObservation'
    {_hcoIPAddress = Nothing, _hcoStatusReport = Nothing, _hcoRegion = Nothing}


-- | The IP address of the Amazon Route 53 health checker that provided the failure reason in @StatusReport@ .
hcoIPAddress :: Lens' HealthCheckObservation (Maybe Text)
hcoIPAddress = lens _hcoIPAddress (\ s a -> s{_hcoIPAddress = a})

-- | A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker and the time of the failed health check.
hcoStatusReport :: Lens' HealthCheckObservation (Maybe StatusReport)
hcoStatusReport = lens _hcoStatusReport (\ s a -> s{_hcoStatusReport = a})

-- | The region of the Amazon Route 53 health checker that provided the status in @StatusReport@ .
hcoRegion :: Lens' HealthCheckObservation (Maybe HealthCheckRegion)
hcoRegion = lens _hcoRegion (\ s a -> s{_hcoRegion = a})

instance FromXML HealthCheckObservation where
        parseXML x
          = HealthCheckObservation' <$>
              (x .@? "IPAddress") <*> (x .@? "StatusReport") <*>
                (x .@? "Region")

instance Hashable HealthCheckObservation where

instance NFData HealthCheckObservation where

-- | A complex type that contains general information about the hosted zone.
--
--
--
-- /See:/ 'hostedZone' smart constructor.
data HostedZone = HostedZone'
  { _hzLinkedService          :: !(Maybe LinkedService)
  , _hzConfig                 :: !(Maybe HostedZoneConfig)
  , _hzResourceRecordSetCount :: !(Maybe Integer)
  , _hzId                     :: !ResourceId
  , _hzName                   :: !Text
  , _hzCallerReference        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostedZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzLinkedService' - If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- * 'hzConfig' - A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
--
-- * 'hzResourceRecordSetCount' - The number of resource record sets in the hosted zone.
--
-- * 'hzId' - The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- * 'hzName' - The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar. For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see 'CreateHostedZone' .
--
-- * 'hzCallerReference' - The value that you specified for @CallerReference@ when you created the hosted zone.
hostedZone
    :: ResourceId -- ^ 'hzId'
    -> Text -- ^ 'hzName'
    -> Text -- ^ 'hzCallerReference'
    -> HostedZone
hostedZone pId_ pName_ pCallerReference_ =
  HostedZone'
    { _hzLinkedService = Nothing
    , _hzConfig = Nothing
    , _hzResourceRecordSetCount = Nothing
    , _hzId = pId_
    , _hzName = pName_
    , _hzCallerReference = pCallerReference_
    }


-- | If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Amazon Route 53.
hzLinkedService :: Lens' HostedZone (Maybe LinkedService)
hzLinkedService = lens _hzLinkedService (\ s a -> s{_hzLinkedService = a})

-- | A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
hzConfig :: Lens' HostedZone (Maybe HostedZoneConfig)
hzConfig = lens _hzConfig (\ s a -> s{_hzConfig = a})

-- | The number of resource record sets in the hosted zone.
hzResourceRecordSetCount :: Lens' HostedZone (Maybe Integer)
hzResourceRecordSetCount = lens _hzResourceRecordSetCount (\ s a -> s{_hzResourceRecordSetCount = a})

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
hzId :: Lens' HostedZone ResourceId
hzId = lens _hzId (\ s a -> s{_hzId = a})

-- | The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar. For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see 'CreateHostedZone' .
hzName :: Lens' HostedZone Text
hzName = lens _hzName (\ s a -> s{_hzName = a})

-- | The value that you specified for @CallerReference@ when you created the hosted zone.
hzCallerReference :: Lens' HostedZone Text
hzCallerReference = lens _hzCallerReference (\ s a -> s{_hzCallerReference = a})

instance FromXML HostedZone where
        parseXML x
          = HostedZone' <$>
              (x .@? "LinkedService") <*> (x .@? "Config") <*>
                (x .@? "ResourceRecordSetCount")
                <*> (x .@ "Id")
                <*> (x .@ "Name")
                <*> (x .@ "CallerReference")

instance Hashable HostedZone where

instance NFData HostedZone where

-- | A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the @HostedZoneConfig@ and @Comment@ elements.
--
--
--
-- /See:/ 'hostedZoneConfig' smart constructor.
data HostedZoneConfig = HostedZoneConfig'
  { _hzcPrivateZone :: !(Maybe Bool)
  , _hzcComment     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostedZoneConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzcPrivateZone' - A value that indicates whether this is a private hosted zone.
--
-- * 'hzcComment' - Any comments that you want to include about the hosted zone.
hostedZoneConfig
    :: HostedZoneConfig
hostedZoneConfig =
  HostedZoneConfig' {_hzcPrivateZone = Nothing, _hzcComment = Nothing}


-- | A value that indicates whether this is a private hosted zone.
hzcPrivateZone :: Lens' HostedZoneConfig (Maybe Bool)
hzcPrivateZone = lens _hzcPrivateZone (\ s a -> s{_hzcPrivateZone = a})

-- | Any comments that you want to include about the hosted zone.
hzcComment :: Lens' HostedZoneConfig (Maybe Text)
hzcComment = lens _hzcComment (\ s a -> s{_hzcComment = a})

instance FromXML HostedZoneConfig where
        parseXML x
          = HostedZoneConfig' <$>
              (x .@? "PrivateZone") <*> (x .@? "Comment")

instance Hashable HostedZoneConfig where

instance NFData HostedZoneConfig where

instance ToXML HostedZoneConfig where
        toXML HostedZoneConfig'{..}
          = mconcat
              ["PrivateZone" @= _hzcPrivateZone,
               "Comment" @= _hzcComment]

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
--
--
-- /See:/ 'hostedZoneLimit' smart constructor.
data HostedZoneLimit = HostedZoneLimit'
  { _hzlType  :: !HostedZoneLimitType
  , _hzlValue :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HostedZoneLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hzlType' - The limit that you requested. Valid values include the following:     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
-- * 'hzlValue' - The current value for the limit that is specified by @Type@ .
hostedZoneLimit
    :: HostedZoneLimitType -- ^ 'hzlType'
    -> Natural -- ^ 'hzlValue'
    -> HostedZoneLimit
hostedZoneLimit pType_ pValue_ =
  HostedZoneLimit' {_hzlType = pType_, _hzlValue = _Nat # pValue_}


-- | The limit that you requested. Valid values include the following:     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
hzlType :: Lens' HostedZoneLimit HostedZoneLimitType
hzlType = lens _hzlType (\ s a -> s{_hzlType = a})

-- | The current value for the limit that is specified by @Type@ .
hzlValue :: Lens' HostedZoneLimit Natural
hzlValue = lens _hzlValue (\ s a -> s{_hzlValue = a}) . _Nat

instance FromXML HostedZoneLimit where
        parseXML x
          = HostedZoneLimit' <$>
              (x .@ "Type") <*> (x .@ "Value")

instance Hashable HostedZoneLimit where

instance NFData HostedZoneLimit where

-- | If a health check or hosted zone was created by another service, @LinkedService@ is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
--
--
-- /See:/ 'linkedService' smart constructor.
data LinkedService = LinkedService'
  { _lsServicePrincipal :: !(Maybe Text)
  , _lsDescription      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LinkedService' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsServicePrincipal' - If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
--
-- * 'lsDescription' - If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
linkedService
    :: LinkedService
linkedService =
  LinkedService' {_lsServicePrincipal = Nothing, _lsDescription = Nothing}


-- | If the health check or hosted zone was created by another service, the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
lsServicePrincipal :: Lens' LinkedService (Maybe Text)
lsServicePrincipal = lens _lsServicePrincipal (\ s a -> s{_lsServicePrincipal = a})

-- | If the health check or hosted zone was created by another service, an optional description that can be provided by the other service. When a resource is created by another service, you can't edit or delete it using Amazon Route 53.
lsDescription :: Lens' LinkedService (Maybe Text)
lsDescription = lens _lsDescription (\ s a -> s{_lsDescription = a})

instance FromXML LinkedService where
        parseXML x
          = LinkedService' <$>
              (x .@? "ServicePrincipal") <*> (x .@? "Description")

instance Hashable LinkedService where

instance NFData LinkedService where

-- | A complex type that contains information about a configuration for DNS query logging.
--
--
--
-- /See:/ 'queryLoggingConfig' smart constructor.
data QueryLoggingConfig = QueryLoggingConfig'
  { _qlcId                        :: !Text
  , _qlcHostedZoneId              :: !ResourceId
  , _qlcCloudWatchLogsLogGroupARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'QueryLoggingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qlcId' - The ID for a configuration for DNS query logging.
--
-- * 'qlcHostedZoneId' - The ID of the hosted zone that CloudWatch Logs is logging queries for.
--
-- * 'qlcCloudWatchLogsLogGroupARN' - The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
queryLoggingConfig
    :: Text -- ^ 'qlcId'
    -> ResourceId -- ^ 'qlcHostedZoneId'
    -> Text -- ^ 'qlcCloudWatchLogsLogGroupARN'
    -> QueryLoggingConfig
queryLoggingConfig pId_ pHostedZoneId_ pCloudWatchLogsLogGroupARN_ =
  QueryLoggingConfig'
    { _qlcId = pId_
    , _qlcHostedZoneId = pHostedZoneId_
    , _qlcCloudWatchLogsLogGroupARN = pCloudWatchLogsLogGroupARN_
    }


-- | The ID for a configuration for DNS query logging.
qlcId :: Lens' QueryLoggingConfig Text
qlcId = lens _qlcId (\ s a -> s{_qlcId = a})

-- | The ID of the hosted zone that CloudWatch Logs is logging queries for.
qlcHostedZoneId :: Lens' QueryLoggingConfig ResourceId
qlcHostedZoneId = lens _qlcHostedZoneId (\ s a -> s{_qlcHostedZoneId = a})

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group that Amazon Route 53 is publishing logs to.
qlcCloudWatchLogsLogGroupARN :: Lens' QueryLoggingConfig Text
qlcCloudWatchLogsLogGroupARN = lens _qlcCloudWatchLogsLogGroupARN (\ s a -> s{_qlcCloudWatchLogsLogGroupARN = a})

instance FromXML QueryLoggingConfig where
        parseXML x
          = QueryLoggingConfig' <$>
              (x .@ "Id") <*> (x .@ "HostedZoneId") <*>
                (x .@ "CloudWatchLogsLogGroupArn")

instance Hashable QueryLoggingConfig where

instance NFData QueryLoggingConfig where

-- | Information specific to the resource record.
--
--
--
-- /See:/ 'resourceRecord' smart constructor.
newtype ResourceRecord = ResourceRecord'
  { _rrValue :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrValue' - The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ . You can specify more than one value for all record types except @CNAME@ and @SOA@ .
resourceRecord
    :: Text -- ^ 'rrValue'
    -> ResourceRecord
resourceRecord pValue_ = ResourceRecord' {_rrValue = pValue_}


-- | The current or new DNS record value, not to exceed 4,000 characters. In the case of a @DELETE@ action, if the current value does not match the actual value, an error is returned. For descriptions about how to format @Value@ for different record types, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ . You can specify more than one value for all record types except @CNAME@ and @SOA@ .
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\ s a -> s{_rrValue = a})

instance FromXML ResourceRecord where
        parseXML x = ResourceRecord' <$> (x .@ "Value")

instance Hashable ResourceRecord where

instance NFData ResourceRecord where

instance ToXML ResourceRecord where
        toXML ResourceRecord'{..}
          = mconcat ["Value" @= _rrValue]

-- | Information about the resource record set to create or delete.
--
--
--
-- /See:/ 'resourceRecordSet' smart constructor.
data ResourceRecordSet = ResourceRecordSet'
  { _rrsTTL                     :: !(Maybe Nat)
  , _rrsResourceRecords         :: !(Maybe (List1 ResourceRecord))
  , _rrsAliasTarget             :: !(Maybe AliasTarget)
  , _rrsWeight                  :: !(Maybe Nat)
  , _rrsTrafficPolicyInstanceId :: !(Maybe Text)
  , _rrsSetIdentifier           :: !(Maybe Text)
  , _rrsFailover                :: !(Maybe Failover)
  , _rrsHealthCheckId           :: !(Maybe Text)
  , _rrsRegion                  :: !(Maybe Region)
  , _rrsGeoLocation             :: !(Maybe GeoLocation)
  , _rrsMultiValueAnswer        :: !(Maybe Bool)
  , _rrsName                    :: !Text
  , _rrsType                    :: !RecordType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceRecordSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsTTL' - The resource record cache time to live (TTL), in seconds. Note the following:     * If you're creating or updating an alias resource record set, omit @TTL@ . Amazon Route 53 uses the value of @TTL@ for the alias target.      * If you're associating this resource record set with a health check (if you're adding a @HealthCheckId@ element), we recommend that you specify a @TTL@ of 60 seconds or less so clients respond quickly to changes in health status.     * All of the resource record sets in a group of weighted resource record sets must have the same value for @TTL@ .     * If a group of weighted resource record sets includes one or more weighted alias resource record sets for which the alias target is an ELB load balancer, we recommend that you specify a @TTL@ of 60 seconds for all of the non-alias weighted resource record sets that have the same name and type. Values other than 60 seconds (the TTL for load balancers) will change the effect of the values that you specify for @Weight@ .
--
-- * 'rrsResourceRecords' - Information about the resource records to act upon.
--
-- * 'rrsAliasTarget' - /Alias resource record sets only:/ Information about the CloudFront distribution, AWS Elastic Beanstalk environment, ELB load balancer, Amazon S3 bucket, or Amazon Route 53 resource record set to which you're redirecting queries. The AWS Elastic Beanstalk environment must have a regionalized subdomain. If you're creating resource records sets for a private hosted zone, note the following:     * You can't create alias resource record sets for CloudFront distributions in a private hosted zone.     * Creating geolocation alias resource record sets or latency alias resource record sets in a private hosted zone is unsupported.     * For information about creating failover resource record sets in a private hosted zone, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone> in the /Amazon Route 53 Developer Guide/ .
--
-- * 'rrsWeight' - /Weighted resource record sets only:/ Among resource record sets that have the same combination of DNS name and type, a value that determines the proportion of DNS queries that Amazon Route 53 responds to using the current resource record set. Amazon Route 53 calculates the sum of the weights for the resource record sets that have the same combination of DNS name and type. Amazon Route 53 then responds to queries based on the ratio of a resource's weight to the total. Note the following:     * You must specify a value for the @Weight@ element for every weighted resource record set.     * You can only specify one @ResourceRecord@ per weighted resource record set.     * You can't create latency, failover, or geolocation resource record sets that have the same values for the @Name@ and @Type@ elements as weighted resource record sets.     * You can create a maximum of 100 weighted resource record sets that have the same values for the @Name@ and @Type@ elements.     * For weighted (but not weighted alias) resource record sets, if you set @Weight@ to @0@ for a resource record set, Amazon Route 53 never responds to queries with the applicable value for that resource record set. However, if you set @Weight@ to @0@ for all resource record sets that have the same combination of DNS name and type, traffic is routed to all resources with equal probability. The effect of setting @Weight@ to @0@ is different when you associate health checks with weighted resource record sets. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-configuring-options.html Options for Configuring Amazon Route 53 Active-Active and Active-Passive Failover> in the /Amazon Route 53 Developer Guide/ .
--
-- * 'rrsTrafficPolicyInstanceId' - When you create a traffic policy instance, Amazon Route 53 automatically creates a resource record set. @TrafficPolicyInstanceId@ is the ID of the traffic policy instance that Amazon Route 53 created this resource record set for. /Important:/ To delete the resource record set that is associated with a traffic policy instance, use @DeleteTrafficPolicyInstance@ . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Amazon Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.
--
-- * 'rrsSetIdentifier' - /Weighted, Latency, Geo, and Failover resource record sets only:/ An identifier that differentiates among multiple resource record sets that have the same combination of DNS name and type. The value of @SetIdentifier@ must be unique for each resource record set that has the same combination of DNS name and type. Omit @SetIdentifier@ for any other types of record sets.
--
-- * 'rrsFailover' - /Failover resource record sets only:/ To configure failover, you add the @Failover@ element to two resource record sets. For one resource record set, you specify @PRIMARY@ as the value for @Failover@ ; for the other resource record set, you specify @SECONDARY@ . In addition, you include the @HealthCheckId@ element and specify the health check that you want Amazon Route 53 to perform for each resource record set. Except where noted, the following failover behaviors assume that you have included the @HealthCheckId@ element in both resource record sets:     * When the primary resource record set is healthy, Amazon Route 53 responds to DNS queries with the applicable value from the primary resource record set regardless of the health of the secondary resource record set.     * When the primary resource record set is unhealthy and the secondary resource record set is healthy, Amazon Route 53 responds to DNS queries with the applicable value from the secondary resource record set.     * When the secondary resource record set is unhealthy, Amazon Route 53 responds to DNS queries with the applicable value from the primary resource record set regardless of the health of the primary resource record set.     * If you omit the @HealthCheckId@ element for the secondary resource record set, and if the primary resource record set is unhealthy, Amazon Route 53 always responds to DNS queries with the applicable value from the secondary resource record set. This is true regardless of the health of the associated endpoint. You can't create non-failover resource record sets that have the same values for the @Name@ and @Type@ elements as failover resource record sets. For failover alias resource record sets, you must also include the @EvaluateTargetHealth@ element and set the value to true. For more information about configuring failover for Amazon Route 53, see the following topics in the /Amazon Route 53 Developer Guide/ :      * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover>      * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone>
--
-- * 'rrsHealthCheckId' - If you want Amazon Route 53 to return this resource record set in response to a DNS query only when a health check is passing, include the @HealthCheckId@ element and specify the ID of the applicable health check. Amazon Route 53 determines whether a resource record set is healthy based on one of the following:     * By periodically sending a request to the endpoint that is specified in the health check     * By aggregating the status of a specified group of health checks (calculated health checks)     * By determining the current state of a CloudWatch alarm (CloudWatch metric health checks) For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> . The @HealthCheckId@ element is only useful when Amazon Route 53 is choosing between two or more resource record sets to respond to a DNS query, and you want Amazon Route 53 to base the choice in part on the status of a health check. Configuring health checks only makes sense in the following configurations:     * You're checking the health of the resource record sets in a group of weighted, latency, geolocation, or failover resource record sets, and you specify health check IDs for all of the resource record sets. If the health check for one resource record set specifies an endpoint that is not healthy, Amazon Route 53 stops responding to queries using the value for that resource record set.     * You set @EvaluateTargetHealth@ to true for the resource record sets in a group of alias, weighted alias, latency alias, geolocation alias, or failover alias resource record sets, and you specify health check IDs for all of the resource record sets that are referenced by the alias resource record sets. /Important:/ Amazon Route 53 doesn't check the health of the endpoint specified in the resource record set, for example, the endpoint specified by the IP address in the @Value@ element. When you add a @HealthCheckId@ element to a resource record set, Amazon Route 53 checks the health of the endpoint that you specified in the health check.  For geolocation resource record sets, if an endpoint is unhealthy, Amazon Route 53 looks for a resource record set for the larger, associated geographic region. For example, suppose you have resource record sets for a state in the United States, for the United States, for North America, and for all locations. If the endpoint for the state resource record set is unhealthy, Amazon Route 53 checks the resource record sets for the United States, for North America, and for all locations (a resource record set for which the value of @CountryCode@ is @*@ ), in that order, until it finds a resource record set for which the endpoint is healthy.  If your health checks specify the endpoint only by domain name, we recommend that you create a separate health check for each endpoint. For example, create a health check for each @HTTP@ server that is serving content for @www.example.com@ . For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-2-www.example.com@ ), not the name of the resource record sets (example.com). /Important:/ n this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and then associate the health check with those resource record sets, health check results will be unpredictable. For more information, see the following topics in the /Amazon Route 53 Developer Guide/ :     * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover>      * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone>
--
-- * 'rrsRegion' - /Latency-based resource record sets only:/ The Amazon EC2 Region where you created the resource that this resource record set refers to. The resource typically is an AWS resource, such as an EC2 instance or an ELB load balancer, and is referred to by an IP address or a DNS domain name, depending on the record type. When Amazon Route 53 receives a DNS query for a domain name and type for which you have created latency resource record sets, Amazon Route 53 selects the latency resource record set that has the lowest latency between the end user and the associated Amazon EC2 Region. Amazon Route 53 then returns the value that is associated with the selected resource record set. Note the following:     * You can only specify one @ResourceRecord@ per latency resource record set.     * You can only create one latency resource record set for each Amazon EC2 Region.     * You aren't required to create latency resource record sets for all Amazon EC2 Regions. Amazon Route 53 will choose the region with the best latency from among the regions that you create latency resource record sets for.     * You can't create non-latency resource record sets that have the same values for the @Name@ and @Type@ elements as latency resource record sets.
--
-- * 'rrsGeoLocation' - /Geo location resource record sets only:/ A complex type that lets you control how Amazon Route 53 responds to DNS queries based on the geographic origin of the query. For example, if you want all queries from Africa to be routed to a web server with an IP address of @192.0.2.111@ , create a resource record set with a @Type@ of @A@ and a @ContinentCode@ of @AF@ . If you create separate resource record sets for overlapping geographic regions (for example, one resource record set for a continent and one for a country on the same continent), priority goes to the smallest geographic region. This allows you to route most queries for a continent to one resource and to route queries for a country on that continent to a different resource. You can't create two geolocation resource record sets that specify the same geographic location. The value @*@ in the @CountryCode@ element matches all geographic locations that aren't specified in other geolocation resource record sets that have the same values for the @Name@ and @Type@ elements. /Important:/ Geolocation works by mapping IP addresses to locations. However, some IP addresses aren't mapped to geographic locations, so even if you create geolocation resource record sets that cover all seven continents, Amazon Route 53 will receive some DNS queries from locations that it can't identify. We recommend that you create a resource record set for which the value of @CountryCode@ is @*@ , which handles both queries that come from locations for which you haven't created geolocation resource record sets and queries from IP addresses that aren't mapped to a location. If you don't create a @*@ resource record set, Amazon Route 53 returns a "no answer" response for queries from those locations. You can't create non-geolocation resource record sets that have the same values for the @Name@ and @Type@ elements as geolocation resource record sets.
--
-- * 'rrsMultiValueAnswer' - /Multivalue answer resource record sets only/ : To route traffic approximately randomly to multiple resources, such as web servers, create one multivalue answer record for each resource and specify @true@ for @MultiValueAnswer@ . Note the following:     * If you associate a health check with a multivalue answer resource record set, Amazon Route 53 responds to DNS queries with the corresponding IP address only when the health check is healthy.     * If you don't associate a health check with a multivalue answer record, Amazon Route 53 always considers the record to be healthy.     * Amazon Route 53 responds to DNS queries with up to eight healthy records; if you have eight or fewer healthy records, Amazon Route 53 responds to all DNS queries with all the healthy records.     * If you have more than eight healthy records, Amazon Route 53 responds to different DNS resolvers with different combinations of healthy records.     * When all records are unhealthy, Amazon Route 53 responds to DNS queries with up to eight unhealthy records.     * If a resource becomes unavailable after a resolver caches a response, client software typically tries another of the IP addresses in the response. You can't create multivalue answer alias records.
--
-- * 'rrsName' - The name of the domain you want to perform the action on. Enter a fully qualified domain name, for example, @www.example.com@ . You can optionally include a trailing dot. If you omit the trailing dot, Amazon Route 53 still assumes that the domain name that you specify is fully qualified. This means that Amazon Route 53 treats @www.example.com@ (without a trailing dot) and @www.example.com.@ (with a trailing dot) as identical. For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ . You can use the asterisk (*) wildcard to replace the leftmost label in a domain name, for example, @*.example.com@ . Note the following:     * The * must replace the entire label. For example, you can't specify @*prod.example.com@ or @prod*.example.com@ .     * The * can't replace any of the middle labels, for example, marketing.*.example.com.     * If you include * in any position other than the leftmost label in a domain name, DNS treats it as an * character (ASCII 42), not as a wildcard. /Important:/ You can't use the * wildcard for resource records sets that have a type of NS. You can use the * wildcard as the leftmost label in a domain name, for example, @*.example.com@ . You can't use an * for one of the middle labels, for example, @marketing.*.example.com@ . In addition, the * must replace the entire label; for example, you can't specify @prod*.example.com@ .
--
-- * 'rrsType' - The DNS record type. For information about different record types and how data is encoded for them, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ . Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@  Values for weighted, latency, geolocation, and failover resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ | @SRV@ | @TXT@ . When creating a group of weighted, latency, geolocation, or failover resource record sets, specify the same value for all of the resource record sets in the group. Valid values for multivalue answer resource record sets: @A@ | @AAAA@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ | @SRV@ | @TXT@  Values for alias resource record sets:     * __CloudFront distributions:__ @A@  If IPv6 is enabled for the distribution, create two resource record sets to route traffic to your distribution, one with a value of @A@ and one with a value of @AAAA@ .      * __AWS Elastic Beanstalk environment that has a regionalized subdomain__ : @A@      * __ELB load balancers:__ @A@ | @AAAA@      * __Amazon S3 buckets:__ @A@      * __Another resource record set in this hosted zone:__ Specify the type of the resource record set that you're creating the alias for. All values are supported except @NS@ and @SOA@ .
resourceRecordSet
    :: Text -- ^ 'rrsName'
    -> RecordType -- ^ 'rrsType'
    -> ResourceRecordSet
resourceRecordSet pName_ pType_ =
  ResourceRecordSet'
    { _rrsTTL = Nothing
    , _rrsResourceRecords = Nothing
    , _rrsAliasTarget = Nothing
    , _rrsWeight = Nothing
    , _rrsTrafficPolicyInstanceId = Nothing
    , _rrsSetIdentifier = Nothing
    , _rrsFailover = Nothing
    , _rrsHealthCheckId = Nothing
    , _rrsRegion = Nothing
    , _rrsGeoLocation = Nothing
    , _rrsMultiValueAnswer = Nothing
    , _rrsName = pName_
    , _rrsType = pType_
    }


-- | The resource record cache time to live (TTL), in seconds. Note the following:     * If you're creating or updating an alias resource record set, omit @TTL@ . Amazon Route 53 uses the value of @TTL@ for the alias target.      * If you're associating this resource record set with a health check (if you're adding a @HealthCheckId@ element), we recommend that you specify a @TTL@ of 60 seconds or less so clients respond quickly to changes in health status.     * All of the resource record sets in a group of weighted resource record sets must have the same value for @TTL@ .     * If a group of weighted resource record sets includes one or more weighted alias resource record sets for which the alias target is an ELB load balancer, we recommend that you specify a @TTL@ of 60 seconds for all of the non-alias weighted resource record sets that have the same name and type. Values other than 60 seconds (the TTL for load balancers) will change the effect of the values that you specify for @Weight@ .
rrsTTL :: Lens' ResourceRecordSet (Maybe Natural)
rrsTTL = lens _rrsTTL (\ s a -> s{_rrsTTL = a}) . mapping _Nat

-- | Information about the resource records to act upon.
rrsResourceRecords :: Lens' ResourceRecordSet (Maybe (NonEmpty ResourceRecord))
rrsResourceRecords = lens _rrsResourceRecords (\ s a -> s{_rrsResourceRecords = a}) . mapping _List1

-- | /Alias resource record sets only:/ Information about the CloudFront distribution, AWS Elastic Beanstalk environment, ELB load balancer, Amazon S3 bucket, or Amazon Route 53 resource record set to which you're redirecting queries. The AWS Elastic Beanstalk environment must have a regionalized subdomain. If you're creating resource records sets for a private hosted zone, note the following:     * You can't create alias resource record sets for CloudFront distributions in a private hosted zone.     * Creating geolocation alias resource record sets or latency alias resource record sets in a private hosted zone is unsupported.     * For information about creating failover resource record sets in a private hosted zone, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone> in the /Amazon Route 53 Developer Guide/ .
rrsAliasTarget :: Lens' ResourceRecordSet (Maybe AliasTarget)
rrsAliasTarget = lens _rrsAliasTarget (\ s a -> s{_rrsAliasTarget = a})

-- | /Weighted resource record sets only:/ Among resource record sets that have the same combination of DNS name and type, a value that determines the proportion of DNS queries that Amazon Route 53 responds to using the current resource record set. Amazon Route 53 calculates the sum of the weights for the resource record sets that have the same combination of DNS name and type. Amazon Route 53 then responds to queries based on the ratio of a resource's weight to the total. Note the following:     * You must specify a value for the @Weight@ element for every weighted resource record set.     * You can only specify one @ResourceRecord@ per weighted resource record set.     * You can't create latency, failover, or geolocation resource record sets that have the same values for the @Name@ and @Type@ elements as weighted resource record sets.     * You can create a maximum of 100 weighted resource record sets that have the same values for the @Name@ and @Type@ elements.     * For weighted (but not weighted alias) resource record sets, if you set @Weight@ to @0@ for a resource record set, Amazon Route 53 never responds to queries with the applicable value for that resource record set. However, if you set @Weight@ to @0@ for all resource record sets that have the same combination of DNS name and type, traffic is routed to all resources with equal probability. The effect of setting @Weight@ to @0@ is different when you associate health checks with weighted resource record sets. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-configuring-options.html Options for Configuring Amazon Route 53 Active-Active and Active-Passive Failover> in the /Amazon Route 53 Developer Guide/ .
rrsWeight :: Lens' ResourceRecordSet (Maybe Natural)
rrsWeight = lens _rrsWeight (\ s a -> s{_rrsWeight = a}) . mapping _Nat

-- | When you create a traffic policy instance, Amazon Route 53 automatically creates a resource record set. @TrafficPolicyInstanceId@ is the ID of the traffic policy instance that Amazon Route 53 created this resource record set for. /Important:/ To delete the resource record set that is associated with a traffic policy instance, use @DeleteTrafficPolicyInstance@ . Amazon Route 53 will delete the resource record set automatically. If you delete the resource record set by using @ChangeResourceRecordSets@ , Amazon Route 53 doesn't automatically delete the traffic policy instance, and you'll continue to be charged for it even though it's no longer in use.
rrsTrafficPolicyInstanceId :: Lens' ResourceRecordSet (Maybe Text)
rrsTrafficPolicyInstanceId = lens _rrsTrafficPolicyInstanceId (\ s a -> s{_rrsTrafficPolicyInstanceId = a})

-- | /Weighted, Latency, Geo, and Failover resource record sets only:/ An identifier that differentiates among multiple resource record sets that have the same combination of DNS name and type. The value of @SetIdentifier@ must be unique for each resource record set that has the same combination of DNS name and type. Omit @SetIdentifier@ for any other types of record sets.
rrsSetIdentifier :: Lens' ResourceRecordSet (Maybe Text)
rrsSetIdentifier = lens _rrsSetIdentifier (\ s a -> s{_rrsSetIdentifier = a})

-- | /Failover resource record sets only:/ To configure failover, you add the @Failover@ element to two resource record sets. For one resource record set, you specify @PRIMARY@ as the value for @Failover@ ; for the other resource record set, you specify @SECONDARY@ . In addition, you include the @HealthCheckId@ element and specify the health check that you want Amazon Route 53 to perform for each resource record set. Except where noted, the following failover behaviors assume that you have included the @HealthCheckId@ element in both resource record sets:     * When the primary resource record set is healthy, Amazon Route 53 responds to DNS queries with the applicable value from the primary resource record set regardless of the health of the secondary resource record set.     * When the primary resource record set is unhealthy and the secondary resource record set is healthy, Amazon Route 53 responds to DNS queries with the applicable value from the secondary resource record set.     * When the secondary resource record set is unhealthy, Amazon Route 53 responds to DNS queries with the applicable value from the primary resource record set regardless of the health of the primary resource record set.     * If you omit the @HealthCheckId@ element for the secondary resource record set, and if the primary resource record set is unhealthy, Amazon Route 53 always responds to DNS queries with the applicable value from the secondary resource record set. This is true regardless of the health of the associated endpoint. You can't create non-failover resource record sets that have the same values for the @Name@ and @Type@ elements as failover resource record sets. For failover alias resource record sets, you must also include the @EvaluateTargetHealth@ element and set the value to true. For more information about configuring failover for Amazon Route 53, see the following topics in the /Amazon Route 53 Developer Guide/ :      * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover>      * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone>
rrsFailover :: Lens' ResourceRecordSet (Maybe Failover)
rrsFailover = lens _rrsFailover (\ s a -> s{_rrsFailover = a})

-- | If you want Amazon Route 53 to return this resource record set in response to a DNS query only when a health check is passing, include the @HealthCheckId@ element and specify the ID of the applicable health check. Amazon Route 53 determines whether a resource record set is healthy based on one of the following:     * By periodically sending a request to the endpoint that is specified in the health check     * By aggregating the status of a specified group of health checks (calculated health checks)     * By determining the current state of a CloudWatch alarm (CloudWatch metric health checks) For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-determining-health-of-endpoints.html How Amazon Route 53 Determines Whether an Endpoint Is Healthy> . The @HealthCheckId@ element is only useful when Amazon Route 53 is choosing between two or more resource record sets to respond to a DNS query, and you want Amazon Route 53 to base the choice in part on the status of a health check. Configuring health checks only makes sense in the following configurations:     * You're checking the health of the resource record sets in a group of weighted, latency, geolocation, or failover resource record sets, and you specify health check IDs for all of the resource record sets. If the health check for one resource record set specifies an endpoint that is not healthy, Amazon Route 53 stops responding to queries using the value for that resource record set.     * You set @EvaluateTargetHealth@ to true for the resource record sets in a group of alias, weighted alias, latency alias, geolocation alias, or failover alias resource record sets, and you specify health check IDs for all of the resource record sets that are referenced by the alias resource record sets. /Important:/ Amazon Route 53 doesn't check the health of the endpoint specified in the resource record set, for example, the endpoint specified by the IP address in the @Value@ element. When you add a @HealthCheckId@ element to a resource record set, Amazon Route 53 checks the health of the endpoint that you specified in the health check.  For geolocation resource record sets, if an endpoint is unhealthy, Amazon Route 53 looks for a resource record set for the larger, associated geographic region. For example, suppose you have resource record sets for a state in the United States, for the United States, for North America, and for all locations. If the endpoint for the state resource record set is unhealthy, Amazon Route 53 checks the resource record sets for the United States, for North America, and for all locations (a resource record set for which the value of @CountryCode@ is @*@ ), in that order, until it finds a resource record set for which the endpoint is healthy.  If your health checks specify the endpoint only by domain name, we recommend that you create a separate health check for each endpoint. For example, create a health check for each @HTTP@ server that is serving content for @www.example.com@ . For the value of @FullyQualifiedDomainName@ , specify the domain name of the server (such as @us-east-2-www.example.com@ ), not the name of the resource record sets (example.com). /Important:/ n this configuration, if you create a health check for which the value of @FullyQualifiedDomainName@ matches the name of the resource record sets and then associate the health check with those resource record sets, health check results will be unpredictable. For more information, see the following topics in the /Amazon Route 53 Developer Guide/ :     * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover.html Amazon Route 53 Health Checks and DNS Failover>      * <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html Configuring Failover in a Private Hosted Zone>
rrsHealthCheckId :: Lens' ResourceRecordSet (Maybe Text)
rrsHealthCheckId = lens _rrsHealthCheckId (\ s a -> s{_rrsHealthCheckId = a})

-- | /Latency-based resource record sets only:/ The Amazon EC2 Region where you created the resource that this resource record set refers to. The resource typically is an AWS resource, such as an EC2 instance or an ELB load balancer, and is referred to by an IP address or a DNS domain name, depending on the record type. When Amazon Route 53 receives a DNS query for a domain name and type for which you have created latency resource record sets, Amazon Route 53 selects the latency resource record set that has the lowest latency between the end user and the associated Amazon EC2 Region. Amazon Route 53 then returns the value that is associated with the selected resource record set. Note the following:     * You can only specify one @ResourceRecord@ per latency resource record set.     * You can only create one latency resource record set for each Amazon EC2 Region.     * You aren't required to create latency resource record sets for all Amazon EC2 Regions. Amazon Route 53 will choose the region with the best latency from among the regions that you create latency resource record sets for.     * You can't create non-latency resource record sets that have the same values for the @Name@ and @Type@ elements as latency resource record sets.
rrsRegion :: Lens' ResourceRecordSet (Maybe Region)
rrsRegion = lens _rrsRegion (\ s a -> s{_rrsRegion = a})

-- | /Geo location resource record sets only:/ A complex type that lets you control how Amazon Route 53 responds to DNS queries based on the geographic origin of the query. For example, if you want all queries from Africa to be routed to a web server with an IP address of @192.0.2.111@ , create a resource record set with a @Type@ of @A@ and a @ContinentCode@ of @AF@ . If you create separate resource record sets for overlapping geographic regions (for example, one resource record set for a continent and one for a country on the same continent), priority goes to the smallest geographic region. This allows you to route most queries for a continent to one resource and to route queries for a country on that continent to a different resource. You can't create two geolocation resource record sets that specify the same geographic location. The value @*@ in the @CountryCode@ element matches all geographic locations that aren't specified in other geolocation resource record sets that have the same values for the @Name@ and @Type@ elements. /Important:/ Geolocation works by mapping IP addresses to locations. However, some IP addresses aren't mapped to geographic locations, so even if you create geolocation resource record sets that cover all seven continents, Amazon Route 53 will receive some DNS queries from locations that it can't identify. We recommend that you create a resource record set for which the value of @CountryCode@ is @*@ , which handles both queries that come from locations for which you haven't created geolocation resource record sets and queries from IP addresses that aren't mapped to a location. If you don't create a @*@ resource record set, Amazon Route 53 returns a "no answer" response for queries from those locations. You can't create non-geolocation resource record sets that have the same values for the @Name@ and @Type@ elements as geolocation resource record sets.
rrsGeoLocation :: Lens' ResourceRecordSet (Maybe GeoLocation)
rrsGeoLocation = lens _rrsGeoLocation (\ s a -> s{_rrsGeoLocation = a})

-- | /Multivalue answer resource record sets only/ : To route traffic approximately randomly to multiple resources, such as web servers, create one multivalue answer record for each resource and specify @true@ for @MultiValueAnswer@ . Note the following:     * If you associate a health check with a multivalue answer resource record set, Amazon Route 53 responds to DNS queries with the corresponding IP address only when the health check is healthy.     * If you don't associate a health check with a multivalue answer record, Amazon Route 53 always considers the record to be healthy.     * Amazon Route 53 responds to DNS queries with up to eight healthy records; if you have eight or fewer healthy records, Amazon Route 53 responds to all DNS queries with all the healthy records.     * If you have more than eight healthy records, Amazon Route 53 responds to different DNS resolvers with different combinations of healthy records.     * When all records are unhealthy, Amazon Route 53 responds to DNS queries with up to eight unhealthy records.     * If a resource becomes unavailable after a resolver caches a response, client software typically tries another of the IP addresses in the response. You can't create multivalue answer alias records.
rrsMultiValueAnswer :: Lens' ResourceRecordSet (Maybe Bool)
rrsMultiValueAnswer = lens _rrsMultiValueAnswer (\ s a -> s{_rrsMultiValueAnswer = a})

-- | The name of the domain you want to perform the action on. Enter a fully qualified domain name, for example, @www.example.com@ . You can optionally include a trailing dot. If you omit the trailing dot, Amazon Route 53 still assumes that the domain name that you specify is fully qualified. This means that Amazon Route 53 treats @www.example.com@ (without a trailing dot) and @www.example.com.@ (with a trailing dot) as identical. For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ . You can use the asterisk (*) wildcard to replace the leftmost label in a domain name, for example, @*.example.com@ . Note the following:     * The * must replace the entire label. For example, you can't specify @*prod.example.com@ or @prod*.example.com@ .     * The * can't replace any of the middle labels, for example, marketing.*.example.com.     * If you include * in any position other than the leftmost label in a domain name, DNS treats it as an * character (ASCII 42), not as a wildcard. /Important:/ You can't use the * wildcard for resource records sets that have a type of NS. You can use the * wildcard as the leftmost label in a domain name, for example, @*.example.com@ . You can't use an * for one of the middle labels, for example, @marketing.*.example.com@ . In addition, the * must replace the entire label; for example, you can't specify @prod*.example.com@ .
rrsName :: Lens' ResourceRecordSet Text
rrsName = lens _rrsName (\ s a -> s{_rrsName = a})

-- | The DNS record type. For information about different record types and how data is encoded for them, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/ResourceRecordTypes.html Supported DNS Resource Record Types> in the /Amazon Route 53 Developer Guide/ . Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@  Values for weighted, latency, geolocation, and failover resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ | @SRV@ | @TXT@ . When creating a group of weighted, latency, geolocation, or failover resource record sets, specify the same value for all of the resource record sets in the group. Valid values for multivalue answer resource record sets: @A@ | @AAAA@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ | @SRV@ | @TXT@  Values for alias resource record sets:     * __CloudFront distributions:__ @A@  If IPv6 is enabled for the distribution, create two resource record sets to route traffic to your distribution, one with a value of @A@ and one with a value of @AAAA@ .      * __AWS Elastic Beanstalk environment that has a regionalized subdomain__ : @A@      * __ELB load balancers:__ @A@ | @AAAA@      * __Amazon S3 buckets:__ @A@      * __Another resource record set in this hosted zone:__ Specify the type of the resource record set that you're creating the alias for. All values are supported except @NS@ and @SOA@ .
rrsType :: Lens' ResourceRecordSet RecordType
rrsType = lens _rrsType (\ s a -> s{_rrsType = a})

instance FromXML ResourceRecordSet where
        parseXML x
          = ResourceRecordSet' <$>
              (x .@? "TTL") <*>
                (x .@? "ResourceRecords" .!@ mempty >>=
                   may (parseXMLList1 "ResourceRecord"))
                <*> (x .@? "AliasTarget")
                <*> (x .@? "Weight")
                <*> (x .@? "TrafficPolicyInstanceId")
                <*> (x .@? "SetIdentifier")
                <*> (x .@? "Failover")
                <*> (x .@? "HealthCheckId")
                <*> (x .@? "Region")
                <*> (x .@? "GeoLocation")
                <*> (x .@? "MultiValueAnswer")
                <*> (x .@ "Name")
                <*> (x .@ "Type")

instance Hashable ResourceRecordSet where

instance NFData ResourceRecordSet where

instance ToXML ResourceRecordSet where
        toXML ResourceRecordSet'{..}
          = mconcat
              ["TTL" @= _rrsTTL,
               "ResourceRecords" @=
                 toXML
                   (toXMLList "ResourceRecord" <$> _rrsResourceRecords),
               "AliasTarget" @= _rrsAliasTarget,
               "Weight" @= _rrsWeight,
               "TrafficPolicyInstanceId" @=
                 _rrsTrafficPolicyInstanceId,
               "SetIdentifier" @= _rrsSetIdentifier,
               "Failover" @= _rrsFailover,
               "HealthCheckId" @= _rrsHealthCheckId,
               "Region" @= _rrsRegion,
               "GeoLocation" @= _rrsGeoLocation,
               "MultiValueAnswer" @= _rrsMultiValueAnswer,
               "Name" @= _rrsName, "Type" @= _rrsType]

-- | A complex type containing a resource and its associated tags.
--
--
--
-- /See:/ 'resourceTagSet' smart constructor.
data ResourceTagSet = ResourceTagSet'
  { _rtsResourceId   :: !(Maybe Text)
  , _rtsResourceType :: !(Maybe TagResourceType)
  , _rtsTags         :: !(Maybe (List1 Tag))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResourceTagSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtsResourceId' - The ID for the specified resource.
--
-- * 'rtsResourceType' - The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
--
-- * 'rtsTags' - The tags associated with the specified resource.
resourceTagSet
    :: ResourceTagSet
resourceTagSet =
  ResourceTagSet'
    {_rtsResourceId = Nothing, _rtsResourceType = Nothing, _rtsTags = Nothing}


-- | The ID for the specified resource.
rtsResourceId :: Lens' ResourceTagSet (Maybe Text)
rtsResourceId = lens _rtsResourceId (\ s a -> s{_rtsResourceId = a})

-- | The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
rtsResourceType :: Lens' ResourceTagSet (Maybe TagResourceType)
rtsResourceType = lens _rtsResourceType (\ s a -> s{_rtsResourceType = a})

-- | The tags associated with the specified resource.
rtsTags :: Lens' ResourceTagSet (Maybe (NonEmpty Tag))
rtsTags = lens _rtsTags (\ s a -> s{_rtsTags = a}) . mapping _List1

instance FromXML ResourceTagSet where
        parseXML x
          = ResourceTagSet' <$>
              (x .@? "ResourceId") <*> (x .@? "ResourceType") <*>
                (x .@? "Tags" .!@ mempty >>=
                   may (parseXMLList1 "Tag"))

instance Hashable ResourceTagSet where

instance NFData ResourceTagSet where

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
--
--
-- /See:/ 'reusableDelegationSetLimit' smart constructor.
data ReusableDelegationSetLimit = ReusableDelegationSetLimit'
  { _rdslType  :: !ReusableDelegationSetLimitType
  , _rdslValue :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReusableDelegationSetLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdslType' - The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- * 'rdslValue' - The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
reusableDelegationSetLimit
    :: ReusableDelegationSetLimitType -- ^ 'rdslType'
    -> Natural -- ^ 'rdslValue'
    -> ReusableDelegationSetLimit
reusableDelegationSetLimit pType_ pValue_ =
  ReusableDelegationSetLimit' {_rdslType = pType_, _rdslValue = _Nat # pValue_}


-- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
rdslType :: Lens' ReusableDelegationSetLimit ReusableDelegationSetLimitType
rdslType = lens _rdslType (\ s a -> s{_rdslType = a})

-- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
rdslValue :: Lens' ReusableDelegationSetLimit Natural
rdslValue = lens _rdslValue (\ s a -> s{_rdslValue = a}) . _Nat

instance FromXML ReusableDelegationSetLimit where
        parseXML x
          = ReusableDelegationSetLimit' <$>
              (x .@ "Type") <*> (x .@ "Value")

instance Hashable ReusableDelegationSetLimit where

instance NFData ReusableDelegationSetLimit where

-- | A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.
--
--
--
-- /See:/ 'statusReport' smart constructor.
data StatusReport = StatusReport'
  { _srStatus      :: !(Maybe Text)
  , _srCheckedTime :: !(Maybe ISO8601)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StatusReport' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srStatus' - A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
--
-- * 'srCheckedTime' - The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
statusReport
    :: StatusReport
statusReport = StatusReport' {_srStatus = Nothing, _srCheckedTime = Nothing}


-- | A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
srStatus :: Lens' StatusReport (Maybe Text)
srStatus = lens _srStatus (\ s a -> s{_srStatus = a})

-- | The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
srCheckedTime :: Lens' StatusReport (Maybe UTCTime)
srCheckedTime = lens _srCheckedTime (\ s a -> s{_srCheckedTime = a}) . mapping _Time

instance FromXML StatusReport where
        parseXML x
          = StatusReport' <$>
              (x .@? "Status") <*> (x .@? "CheckedTime")

instance Hashable StatusReport where

instance NFData StatusReport where

-- | A complex type that contains information about a tag that you want to add or edit for the specified health check or hosted zone.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of @Value@ depends on the operation that you want to perform:     * __Add a tag to a health check or hosted zone__ : @Value@ is the value that you want to give the new tag.     * __Edit a tag__ : @Value@ is the new value that you want to assign the tag.
--
-- * 'tagKey' - The value of @Key@ depends on the operation that you want to perform:     * __Add a tag to a health check or hosted zone__ : @Key@ is the name that you want to give the new tag.     * __Edit a tag__ : @Key@ is the name of the tag that you want to change the @Value@ for.     * __Delete a key__ : @Key@ is the name of the tag you want to remove.     * __Give a name to a health check__ : Edit the default @Name@ tag. In the Amazon Route 53 console, the list of your health checks includes a __Name__ column that lets you see the name that you've given to each health check.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value of @Value@ depends on the operation that you want to perform:     * __Add a tag to a health check or hosted zone__ : @Value@ is the value that you want to give the new tag.     * __Edit a tag__ : @Value@ is the new value that you want to assign the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The value of @Key@ depends on the operation that you want to perform:     * __Add a tag to a health check or hosted zone__ : @Key@ is the name that you want to give the new tag.     * __Edit a tag__ : @Key@ is the name of the tag that you want to change the @Value@ for.     * __Delete a key__ : @Key@ is the name of the tag you want to remove.     * __Give a name to a health check__ : Edit the default @Name@ tag. In the Amazon Route 53 console, the list of your health checks includes a __Name__ column that lets you see the name that you've given to each health check.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToXML Tag where
        toXML Tag'{..}
          = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]

-- | A complex type that contains settings for a traffic policy.
--
--
--
-- /See:/ 'trafficPolicy' smart constructor.
data TrafficPolicy = TrafficPolicy'
  { _tpComment  :: !(Maybe Text)
  , _tpId       :: !Text
  , _tpVersion  :: !Nat
  , _tpName     :: !Text
  , _tpType     :: !RecordType
  , _tpDocument :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrafficPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpComment' - The comment that you specify in the @CreateTrafficPolicy@ request, if any.
--
-- * 'tpId' - The ID that Amazon Route 53 assigned to a traffic policy when you created it.
--
-- * 'tpVersion' - The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
--
-- * 'tpName' - The name that you specified when you created the traffic policy.
--
-- * 'tpType' - The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- * 'tpDocument' - The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <http://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
trafficPolicy
    :: Text -- ^ 'tpId'
    -> Natural -- ^ 'tpVersion'
    -> Text -- ^ 'tpName'
    -> RecordType -- ^ 'tpType'
    -> Text -- ^ 'tpDocument'
    -> TrafficPolicy
trafficPolicy pId_ pVersion_ pName_ pType_ pDocument_ =
  TrafficPolicy'
    { _tpComment = Nothing
    , _tpId = pId_
    , _tpVersion = _Nat # pVersion_
    , _tpName = pName_
    , _tpType = pType_
    , _tpDocument = pDocument_
    }


-- | The comment that you specify in the @CreateTrafficPolicy@ request, if any.
tpComment :: Lens' TrafficPolicy (Maybe Text)
tpComment = lens _tpComment (\ s a -> s{_tpComment = a})

-- | The ID that Amazon Route 53 assigned to a traffic policy when you created it.
tpId :: Lens' TrafficPolicy Text
tpId = lens _tpId (\ s a -> s{_tpId = a})

-- | The version number that Amazon Route 53 assigns to a traffic policy. For a new traffic policy, the value of @Version@ is always 1.
tpVersion :: Lens' TrafficPolicy Natural
tpVersion = lens _tpVersion (\ s a -> s{_tpVersion = a}) . _Nat

-- | The name that you specified when you created the traffic policy.
tpName :: Lens' TrafficPolicy Text
tpName = lens _tpName (\ s a -> s{_tpName = a})

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
tpType :: Lens' TrafficPolicy RecordType
tpType = lens _tpType (\ s a -> s{_tpType = a})

-- | The definition of a traffic policy in JSON format. You specify the JSON document to use for a new traffic policy in the @CreateTrafficPolicy@ request. For more information about the JSON format, see <http://docs.aws.amazon.com/Route53/latest/APIReference/api-policies-traffic-policy-document-format.html Traffic Policy Document Format> .
tpDocument :: Lens' TrafficPolicy Text
tpDocument = lens _tpDocument (\ s a -> s{_tpDocument = a})

instance FromXML TrafficPolicy where
        parseXML x
          = TrafficPolicy' <$>
              (x .@? "Comment") <*> (x .@ "Id") <*>
                (x .@ "Version")
                <*> (x .@ "Name")
                <*> (x .@ "Type")
                <*> (x .@ "Document")

instance Hashable TrafficPolicy where

instance NFData TrafficPolicy where

-- | A complex type that contains settings for the new traffic policy instance.
--
--
--
-- /See:/ 'trafficPolicyInstance' smart constructor.
data TrafficPolicyInstance = TrafficPolicyInstance'
  { _tpiId                   :: !Text
  , _tpiHostedZoneId         :: !ResourceId
  , _tpiName                 :: !Text
  , _tpiTTL                  :: !Nat
  , _tpiState                :: !Text
  , _tpiMessage              :: !Text
  , _tpiTrafficPolicyId      :: !Text
  , _tpiTrafficPolicyVersion :: !Nat
  , _tpiTrafficPolicyType    :: !RecordType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrafficPolicyInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpiId' - The ID that Amazon Route 53 assigned to the new traffic policy instance.
--
-- * 'tpiHostedZoneId' - The ID of the hosted zone that Amazon Route 53 created resource record sets in.
--
-- * 'tpiName' - The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
--
-- * 'tpiTTL' - The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
--
-- * 'tpiState' - The value of @State@ is one of the following values:     * Applied    * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Amazon Route 53 edge locations.     * Creating    * Amazon Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.     * Failed    * Amazon Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
--
-- * 'tpiMessage' - If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
--
-- * 'tpiTrafficPolicyId' - The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- * 'tpiTrafficPolicyVersion' - The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
--
-- * 'tpiTrafficPolicyType' - The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
trafficPolicyInstance
    :: Text -- ^ 'tpiId'
    -> ResourceId -- ^ 'tpiHostedZoneId'
    -> Text -- ^ 'tpiName'
    -> Natural -- ^ 'tpiTTL'
    -> Text -- ^ 'tpiState'
    -> Text -- ^ 'tpiMessage'
    -> Text -- ^ 'tpiTrafficPolicyId'
    -> Natural -- ^ 'tpiTrafficPolicyVersion'
    -> RecordType -- ^ 'tpiTrafficPolicyType'
    -> TrafficPolicyInstance
trafficPolicyInstance pId_ pHostedZoneId_ pName_ pTTL_ pState_ pMessage_ pTrafficPolicyId_ pTrafficPolicyVersion_ pTrafficPolicyType_ =
  TrafficPolicyInstance'
    { _tpiId = pId_
    , _tpiHostedZoneId = pHostedZoneId_
    , _tpiName = pName_
    , _tpiTTL = _Nat # pTTL_
    , _tpiState = pState_
    , _tpiMessage = pMessage_
    , _tpiTrafficPolicyId = pTrafficPolicyId_
    , _tpiTrafficPolicyVersion = _Nat # pTrafficPolicyVersion_
    , _tpiTrafficPolicyType = pTrafficPolicyType_
    }


-- | The ID that Amazon Route 53 assigned to the new traffic policy instance.
tpiId :: Lens' TrafficPolicyInstance Text
tpiId = lens _tpiId (\ s a -> s{_tpiId = a})

-- | The ID of the hosted zone that Amazon Route 53 created resource record sets in.
tpiHostedZoneId :: Lens' TrafficPolicyInstance ResourceId
tpiHostedZoneId = lens _tpiHostedZoneId (\ s a -> s{_tpiHostedZoneId = a})

-- | The DNS name, such as www.example.com, for which Amazon Route 53 responds to queries by using the resource record sets that are associated with this traffic policy instance.
tpiName :: Lens' TrafficPolicyInstance Text
tpiName = lens _tpiName (\ s a -> s{_tpiName = a})

-- | The TTL that Amazon Route 53 assigned to all of the resource record sets that it created in the specified hosted zone.
tpiTTL :: Lens' TrafficPolicyInstance Natural
tpiTTL = lens _tpiTTL (\ s a -> s{_tpiTTL = a}) . _Nat

-- | The value of @State@ is one of the following values:     * Applied    * Amazon Route 53 has finished creating resource record sets, and changes have propagated to all Amazon Route 53 edge locations.     * Creating    * Amazon Route 53 is creating the resource record sets. Use @GetTrafficPolicyInstance@ to confirm that the @CreateTrafficPolicyInstance@ request completed successfully.     * Failed    * Amazon Route 53 wasn't able to create or update the resource record sets. When the value of @State@ is @Failed@ , see @Message@ for an explanation of what caused the request to fail.
tpiState :: Lens' TrafficPolicyInstance Text
tpiState = lens _tpiState (\ s a -> s{_tpiState = a})

-- | If @State@ is @Failed@ , an explanation of the reason for the failure. If @State@ is another value, @Message@ is empty.
tpiMessage :: Lens' TrafficPolicyInstance Text
tpiMessage = lens _tpiMessage (\ s a -> s{_tpiMessage = a})

-- | The ID of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
tpiTrafficPolicyId :: Lens' TrafficPolicyInstance Text
tpiTrafficPolicyId = lens _tpiTrafficPolicyId (\ s a -> s{_tpiTrafficPolicyId = a})

-- | The version of the traffic policy that Amazon Route 53 used to create resource record sets in the specified hosted zone.
tpiTrafficPolicyVersion :: Lens' TrafficPolicyInstance Natural
tpiTrafficPolicyVersion = lens _tpiTrafficPolicyVersion (\ s a -> s{_tpiTrafficPolicyVersion = a}) . _Nat

-- | The DNS type that Amazon Route 53 assigned to all of the resource record sets that it created for this traffic policy instance.
tpiTrafficPolicyType :: Lens' TrafficPolicyInstance RecordType
tpiTrafficPolicyType = lens _tpiTrafficPolicyType (\ s a -> s{_tpiTrafficPolicyType = a})

instance FromXML TrafficPolicyInstance where
        parseXML x
          = TrafficPolicyInstance' <$>
              (x .@ "Id") <*> (x .@ "HostedZoneId") <*>
                (x .@ "Name")
                <*> (x .@ "TTL")
                <*> (x .@ "State")
                <*> (x .@ "Message")
                <*> (x .@ "TrafficPolicyId")
                <*> (x .@ "TrafficPolicyVersion")
                <*> (x .@ "TrafficPolicyType")

instance Hashable TrafficPolicyInstance where

instance NFData TrafficPolicyInstance where

-- | A complex type that contains information about the latest version of one traffic policy that is associated with the current AWS account.
--
--
--
-- /See:/ 'trafficPolicySummary' smart constructor.
data TrafficPolicySummary = TrafficPolicySummary'
  { _tpsId                 :: !Text
  , _tpsName               :: !Text
  , _tpsType               :: !RecordType
  , _tpsLatestVersion      :: !Nat
  , _tpsTrafficPolicyCount :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TrafficPolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpsId' - The ID that Amazon Route 53 assigned to the traffic policy when you created it.
--
-- * 'tpsName' - The name that you specified for the traffic policy when you created it.
--
-- * 'tpsType' - The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
--
-- * 'tpsLatestVersion' - The version number of the latest version of the traffic policy.
--
-- * 'tpsTrafficPolicyCount' - The number of traffic policies that are associated with the current AWS account.
trafficPolicySummary
    :: Text -- ^ 'tpsId'
    -> Text -- ^ 'tpsName'
    -> RecordType -- ^ 'tpsType'
    -> Natural -- ^ 'tpsLatestVersion'
    -> Natural -- ^ 'tpsTrafficPolicyCount'
    -> TrafficPolicySummary
trafficPolicySummary pId_ pName_ pType_ pLatestVersion_ pTrafficPolicyCount_ =
  TrafficPolicySummary'
    { _tpsId = pId_
    , _tpsName = pName_
    , _tpsType = pType_
    , _tpsLatestVersion = _Nat # pLatestVersion_
    , _tpsTrafficPolicyCount = _Nat # pTrafficPolicyCount_
    }


-- | The ID that Amazon Route 53 assigned to the traffic policy when you created it.
tpsId :: Lens' TrafficPolicySummary Text
tpsId = lens _tpsId (\ s a -> s{_tpsId = a})

-- | The name that you specified for the traffic policy when you created it.
tpsName :: Lens' TrafficPolicySummary Text
tpsName = lens _tpsName (\ s a -> s{_tpsName = a})

-- | The DNS type of the resource record sets that Amazon Route 53 creates when you use a traffic policy to create a traffic policy instance.
tpsType :: Lens' TrafficPolicySummary RecordType
tpsType = lens _tpsType (\ s a -> s{_tpsType = a})

-- | The version number of the latest version of the traffic policy.
tpsLatestVersion :: Lens' TrafficPolicySummary Natural
tpsLatestVersion = lens _tpsLatestVersion (\ s a -> s{_tpsLatestVersion = a}) . _Nat

-- | The number of traffic policies that are associated with the current AWS account.
tpsTrafficPolicyCount :: Lens' TrafficPolicySummary Natural
tpsTrafficPolicyCount = lens _tpsTrafficPolicyCount (\ s a -> s{_tpsTrafficPolicyCount = a}) . _Nat

instance FromXML TrafficPolicySummary where
        parseXML x
          = TrafficPolicySummary' <$>
              (x .@ "Id") <*> (x .@ "Name") <*> (x .@ "Type") <*>
                (x .@ "LatestVersion")
                <*> (x .@ "TrafficPolicyCount")

instance Hashable TrafficPolicySummary where

instance NFData TrafficPolicySummary where

-- | (Private hosted zones only) A complex type that contains information about an Amazon VPC.
--
--
--
-- /See:/ 'vpc' smart constructor.
data VPC = VPC'
  { _vpcVPCRegion :: !(Maybe VPCRegion)
  , _vpcVPCId     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VPC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpcVPCRegion' - (Private hosted zones only) The region in which you created an Amazon VPC.
--
-- * 'vpcVPCId' - Undocumented member.
vpc
    :: VPC
vpc = VPC' {_vpcVPCRegion = Nothing, _vpcVPCId = Nothing}


-- | (Private hosted zones only) The region in which you created an Amazon VPC.
vpcVPCRegion :: Lens' VPC (Maybe VPCRegion)
vpcVPCRegion = lens _vpcVPCRegion (\ s a -> s{_vpcVPCRegion = a})

-- | Undocumented member.
vpcVPCId :: Lens' VPC (Maybe Text)
vpcVPCId = lens _vpcVPCId (\ s a -> s{_vpcVPCId = a})

instance FromXML VPC where
        parseXML x
          = VPC' <$> (x .@? "VPCRegion") <*> (x .@? "VPCId")

instance Hashable VPC where

instance NFData VPC where

instance ToXML VPC where
        toXML VPC'{..}
          = mconcat
              ["VPCRegion" @= _vpcVPCRegion, "VPCId" @= _vpcVPCId]
