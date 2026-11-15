---
name: Release Checklist
about: Tasks to complete prior to cutting a new release
title: Release Checklist for *.*
labels: ''
assignees: ''

---
- [ ] Check for new regions on the [AWS Regions Page](https://docs.aws.amazon.com/global-infrastructure/latest/regions/aws-regions.html)
- [ ] Update service bindings that have `Region -> Maybe Text` functions (usually returning an Account ID or Hosted Zone ID). For each function:
      - Check the haddocks for a link in the haddocks showing the table that the function represents. Complain if not.
      - Bring the function into sync with the latest version of the table in the AWS documentation. Note that some of these functions map legacy AWS infrastructure and will not get new regions added to them.
      - This includes `getHostedZoneId :: Region -> Maybe ResourceId` in `Amazonka.Route53.Internal`
- [ ] ~~Update Botocore, if possible~~ (blocked by #888)
- [ ] ~~Configure new services~~ (blocked by #888)
- [ ] Regenerate everything
- [ ] Check against latest GHC versions
- [ ] Add new IMDS paths to our client, based on the [IMDS page in the EC2 User Guide](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-metadata.html).
